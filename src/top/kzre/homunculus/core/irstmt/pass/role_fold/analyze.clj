(ns top.kzre.homunculus.core.irstmt.pass.role-fold.analyze
  "角色分析：给定 function / method 节点，返回该作用域内的 RoleTable。

   纯读——不修改 IR，不做常量折叠。
   每个 function / method 独立分析，不跨函数。
   识别失败静默跳过，不抛异常。

   填充顺序：
     loop-counters → accumulators → collectors → receivers
     if-collectors → if-receivers

   索引识别的核心规则：
     - Accumulator 的写入索引不限形态（变量 / 常量 / 复杂表达式都行），
       只要不含 call 节点（call 可能有副作用，求值结果不确定）。
     - Collector 的每次拷贝 col[Y] = aget(arr, Z) 要求 Y 与 Z 结构相等，
       同样要求 Y 不含 call。
     - 不要求索引是常量，也不要求索引是循环计数器。

   保守边界（识别失败即返回空或跳过）：
     - while body 不是\"单主控 if\"形态 → 放弃该 while 的所有数组角色
     - 索引含 call 节点 → 不识别
     - if-collector 的变量出现在任何 while 的 test 中 → 不识别
     - if-collector 没有对应的 if-receiver → 不识别（交给 DCE）
     - if 的任一分支缺赋值或多次赋值 → 不识别"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.core.irstmt.ast :as ast]
    [top.kzre.homunculus.core.irstmt.pass.role-fold.role :as role]))

;; ═══════════════════════════════════════════
;; 基础谓词 / 提取
;; ═══════════════════════════════════════════

(defn- variable-node? [n]
  (and n (= :variable (ast/kind n))))

(defn- block-node? [n]
  (and n (= :block (ast/kind n))))

(defn- literal-value [n]
  (when (and n (= :literal (ast/kind n))) (:val n)))

(defn- uses-var?
  "表达式节点是否引用了变量 vname。"
  [n vname]
  (letfn [(walk [x]
            (cond
              (nil? x) false
              (= :variable (ast/kind x)) (= vname (:name x))
              :else (boolean (some walk (ast/children x)))))]
    (walk n)))

(defn- contains-call?
  "节点子树中是否包含 :call 节点。
   call 可能有副作用或返回非确定值，含 call 的索引不能做结构相等假设。"
  [n]
  (letfn [(walk [x]
            (cond
              (nil? x) false
              (= :call (ast/kind x)) true
              :else (boolean (some walk (ast/children x)))))]
    (walk n)))

(def ^:private scope-boundary-kinds
  "作用域边界——遇到就不再递归。"
  #{:function :method :record :protocol :protocol-impl})

(defn- collect-nodes-by-kind
  "递归收集 node 下所有指定 kind 的节点。遇到作用域边界停止。"
  [node kind]
  (letfn [(walk [n]
            (cond
              (nil? n) []
              (= kind (ast/kind n)) [n]
              (contains? scope-boundary-kinds (ast/kind n)) []
              :else (mapcat walk (ast/children n))))]
    (walk node)))

;; ═══════════════════════════════════════════
;; 全局收集
;; ═══════════════════════════════════════════

(defn- collect-all
  "递归收集函数体里的 while / if / var-decl。
   返回 {:whiles [...] :ifs [...] :var-decls {name → var-decl}}。"
  [node]
  (letfn [(walk [n acc]
            (cond
              (nil? n) acc
              (contains? scope-boundary-kinds (ast/kind n)) acc

              (= :while (ast/kind n))
              (reduce (fn [a c] (walk c a))
                      (update acc :whiles conj n)
                      (ast/children n))

              (= :if (ast/kind n))
              (reduce (fn [a c] (walk c a))
                      (update acc :ifs conj n)
                      (ast/children n))

              (= :var-decl (ast/kind n))
              (reduce (fn [a c] (walk c a))
                      (update acc :var-decls assoc (:name n) n)
                      (ast/children n))

              :else
              (reduce (fn [a c] (walk c a)) acc (ast/children n))))]
    (walk node {:whiles [] :ifs [] :var-decls {}})))

;; ═══════════════════════════════════════════
;; LoopCounter
;; ═══════════════════════════════════════════

(defn- find-loop-counter
  "在 while 内找 loop counter，返回变量名或 nil。

   条件：
   1. 变量 i 有 var-decl
   2. while 体内有 assign(i, val)，val 引用 i（自增形态 i = i + k / i = i）
   3. i 参与 while 体内某个 if 的 test

   恰好一个候选时返回，多候选放弃。"
  [while-node var-decls]
  (let [body    (:body while-node)
        assigns (collect-nodes-by-kind body :assign)
        ifs     (collect-nodes-by-kind body :if)
        candidates
        (->> assigns
             (keep (fn [a]
                     (let [var (:var a) val (:val a)]
                       (when (variable-node? var)
                         (let [i-name (:name var)]
                           (when (and (uses-var? val i-name)
                                      (contains? var-decls i-name)
                                      (some #(uses-var? (:test %) i-name) ifs))
                             i-name))))))
             set)]
    (when (= 1 (count candidates))
      (first candidates))))

;; ═══════════════════════════════════════════
;; Accumulator
;; ═══════════════════════════════════════════

(defn- find-accumulators
  "在 while 内找 accumulator，返回 [{:var-name :while-node}]。

   条件：while 体内有 aset(target=arr)，arr 有 var-decl，索引不含 call。
   索引不限形态——变量 / 常量 / 复杂表达式均可，只要不含 call。

   多个 arr 可同时识别（同一个 while 里可能有多个 accumulator）。"
  [while-node var-decls]
  (let [body  (:body while-node)
        asets (collect-nodes-by-kind body :aset)
        candidates
        (->> asets
             (keep (fn [a]
                     (let [target (:target a) idx (:idx a)]
                       (when (and (variable-node? target)
                                  (contains? var-decls (:name target))
                                  (not (contains-call? idx)))
                         (:name target)))))
             set)]
    (map (fn [nm]
           {:var-name   nm
            :while-node while-node})
         candidates)))

;; ═══════════════════════════════════════════
;; Collector
;; ═══════════════════════════════════════════

(defn- while-body-main-if
  "找 while body 里的主控 if（忽略 var-decl 等声明）。
   恰好一个 if 时返回；否则返回 nil。"
  [while-node]
  (let [body (:body while-node)]
    (when (block-node? body)
      (let [ifs (filter #(= :if (ast/kind %)) (:stmts body))]
        (when (= 1 (count ifs))
          (first ifs))))))

(defn- node-struct-equal?
  "两个索引节点的结构是否相等。
   用于匹配 col[Y] = aget(arr, Z) 中的 Y 与 Z。
   相等判定：kind 相同 + 递归比较 children。
   不比较 attrs（类型信息可能因推断时机不同而不同）。"
  [a b]
  (letfn [(eq? [x y]
            (cond
              (and (nil? x) (nil? y)) true
              (or (nil? x) (nil? y)) false
              (not= (ast/kind x) (ast/kind y)) false
              :else
              (and (case (ast/kind x)
                     :variable (= (:name x) (:name y))
                     :literal  (= (:val x) (:val y))
                     true)
                   (= (count (ast/children x))
                      (count (ast/children y)))
                   (every? true?
                           (mapv eq? (ast/children x) (ast/children y))))))]
    (eq? a b)))

(defn- find-collectors
  "在 while 退出分支（主控 if 的 else）找 collector。

   条件：
   1. aset(target=variable col, idx=Y, val=aget(target=variable arr, idx=Z))
   2. Y 与 Z 结构相等（node-struct-equal?）
   3. Y 不含 call
   4. col 有 var-decl
   5. arr 是已识别的 accumulator

   不要求 Y 是常量，不要求 indices 完整覆盖。
   语义正确性依赖生成代码的正确性——序列函数生成的 collector
   保证覆盖 accumulator 的所有有效写入槽位。

   返回 [{:var-name :source :indices :aset-nodes :var-decl-node}]，
   其中 indices 是结构去重的索引节点列表（用于诊断，不用于识别判断）。"
  [while-node var-decls accumulators]
  (if-let [main-if (while-body-main-if while-node)]
    (if-let [exit-branch (:else main-if)]
      (let [asets      (collect-nodes-by-kind exit-branch :aset)
            acc-names  (set (map :var-name accumulators))
            matched
            (keep (fn [a]
                    (let [target (:target a) idx (:idx a) val (:val a)]
                      (when (and (variable-node? target)
                                 (not (contains-call? idx))
                                 (= :aget (ast/kind val)))
                        (let [src-target (:target val)
                              src-idx    (:idx val)]
                          (when (and (variable-node? src-target)
                                     (not (contains-call? src-idx))
                                     (contains? acc-names (:name src-target))
                                     (node-struct-equal? idx src-idx))
                            {:col       (:name target)
                             :src       (:name src-target)
                             :idx       idx
                             :aset-node a})))))
                  asets)
            grouped (group-by (juxt :col :src) matched)]
        (for [[[col-name src-name] group] grouped
              :let [col-decl (get var-decls col-name)]
              :when col-decl]
          {:var-name      col-name
           :source        src-name
           :indices       (vec (distinct (map :idx group)))
           :aset-nodes    (mapv :aset-node group)
           :var-decl-node col-decl}))
      [])
    []))

;; ═══════════════════════════════════════════
;; Receiver
;; ═══════════════════════════════════════════

(defn- find-receivers
  "从 var-decls 找 receiver：var-decl(recv, init=(variable col))，col 是 collector。"
  [var-decls collectors]
  (let [col-names (set (keys collectors))]
    (for [[nm vd] var-decls
          :let [init (:val vd)]
          :when (and (variable-node? init)
                     (contains? col-names (:name init)))]
      {:var-name      nm
       :source        (:name init)
       :var-decl-node vd})))

;; ═══════════════════════════════════════════
;; IfCollector / IfReceiver
;; ═══════════════════════════════════════════

(defn- while-test-vars
  "收集所有 while 的 test 直接引用的变量名集合。
   if-collector 的变量若出现在这里，说明它是循环控制标志，不能消除。"
  [whiles]
  (reduce (fn [s w]
            (let [vars (letfn [(walk [x acc]
                                 (cond
                                   (nil? x) acc
                                   (= :variable (ast/kind x))
                                   (conj acc (:name x))
                                   :else
                                   (reduce (fn [a c] (walk c a)) acc (ast/children x))))]
                         (walk (:test w) #{}))]
              (into s vars)))
          #{}
          whiles))

(defn- find-if-receivers
  "从 var-decls 找 if-receiver：var-decl(r, init=(variable t))，t 有 var-decl。"
  [var-decls]
  (for [[nm vd] var-decls
        :let [init (:val vd)]
        :when (variable-node? init)]
    {:var-name      nm
     :source        (:name init)
     :var-decl-node vd}))

(defn- find-if-collectors
  "从 if 找 if-collector。

   条件：
   1. then 和 else 各给 t 赋值一次
   2. t 有 var-decl
   3. t 不在任何 while 的 test 中（排除循环控制标志）
   4. t 有对应的 if-receiver

   不检查 test 是否为常量——`if (true)` / `if (x)` 一视同仁。"
  [ifs var-decls receivers-by-source while-test-vars]
  (for [if-node ifs
        :let [then-assigns (filter #(variable-node? (:var %))
                                   (collect-nodes-by-kind (:then if-node) :assign))
              else-assigns (filter #(variable-node? (:var %))
                                   (collect-nodes-by-kind (:else if-node) :assign))
              then-targets (set (map #(:name (:var %)) then-assigns))
              else-targets (set (map #(:name (:var %)) else-assigns))
              common       (set/intersection then-targets else-targets)]
        t-name common
        :let [then-list (filter #(= t-name (:name (:var %))) then-assigns)
              else-list (filter #(= t-name (:name (:var %))) else-assigns)]
        :when (and (= 1 (count then-list))
                   (= 1 (count else-list))
                   (contains? var-decls t-name)
                   (not (contains? while-test-vars t-name))
                   (contains? receivers-by-source t-name))]
    {:var-name      t-name
     :if-node       if-node
     :then-assign   (first then-list)
     :else-assign   (first else-list)
     :var-decl-node (get var-decls t-name)}))

;; ═══════════════════════════════════════════
;; 主入口
;; ═══════════════════════════════════════════

(defn analyze
  "给定 function / method 节点，返回其作用域内的 RoleTable。"
  [function-or-method-node]
  (let [body (:body function-or-method-node)]
    (if (nil? body)
      (role/empty-table)
      (let [{:keys [whiles ifs var-decls]} (collect-all body)

            ;; ── 数组链 ──
            ;; 1. LoopCounters
            table
            (reduce (fn [t w]
                      (if-let [counter (find-loop-counter w var-decls)]
                        (role/add-loop-counter
                          t (role/->LoopCounter counter w))
                        t))
                    (role/empty-table)
                    whiles)

            ;; 2. Accumulators（每 while 内识别，不依赖 counter）
            table
            (reduce (fn [t w]
                      (reduce (fn [tt acc]
                                (role/add-accumulator
                                  tt (role/->Accumulator
                                       (:var-name acc)
                                       (:while-node acc)
                                       nil)))  ; counter-name 不再必需
                              t
                              (find-accumulators w var-decls)))
                    table
                    whiles)

            ;; 3. Collectors（依赖 accumulators）
            table
            (reduce (fn [t w]
                      (let [accs (filter #(= w (:while-node %))
                                         (vals (:accumulators t)))]
                        (if (seq accs)
                          (reduce (fn [tt c]
                                    (role/add-collector
                                      tt (role/->Collector
                                           (:var-name c) (:source c)
                                           (:indices c)
                                           (:aset-nodes c) (:var-decl-node c))))
                                  t
                                  (find-collectors w var-decls accs))
                          t)))
                    table
                    whiles)

            ;; 4. Receivers（依赖 collectors）
            table
            (reduce (fn [t r]
                      (role/add-receiver
                        t (role/->Receiver (:var-name r) (:source r)
                                           (:var-decl-node r))))
                    table
                    (find-receivers var-decls (:collectors table)))

            ;; ── if 链 ──
            ;; 1. IfReceivers（先行——用于过滤孤立 if-collector）
            raw-if-receivers (find-if-receivers var-decls)
            receivers-by-source (group-by :source raw-if-receivers)

            ;; 2. IfCollectors（过滤：不在 while test 中 + 有 receiver）
            table
            (reduce (fn [t c]
                      (role/add-if-collector
                        t (role/->IfCollector
                            (:var-name c) (:if-node c)
                            (:then-assign c) (:else-assign c)
                            (:var-decl-node c))))
                    table
                    (find-if-collectors ifs var-decls
                                        receivers-by-source
                                        (while-test-vars whiles)))

            ;; 3. IfReceivers 正式登记（只登记 source 是已识别 if-collector 的）
            table
            (reduce (fn [t r]
                      (if (role/if-collector? t (:source r))
                        (role/add-if-receiver
                          t (role/->IfReceiver (:var-name r) (:source r)
                                               (:var-decl-node r)))
                        t))
                    table
                    raw-if-receivers)]
        table))))
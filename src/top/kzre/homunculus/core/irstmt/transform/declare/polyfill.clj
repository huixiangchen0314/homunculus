(ns top.kzre.homunculus.core.irstmt.transform.declare.polyfill
  "var-decl polyfill——CFG 优化之后重建声明。

   ── 策略：块级 LCA 声明 ──

   变量声明在它所有出现（def/use）的最近公共祖先块的开头。
   所有 assign 保持 Assign。

   ── 两遍遍历 ──

   1. 第一遍：收集每个变量的所有出现位置（block 路径）和 attrs
   2. 计算每个变量的 LCA block
   3. 第二遍：在 LCA block 插入 VarDecl——删自赋值——尝试下沉

   ── 声明下沉 ──

   如果 LCA block 里某变量的第一次 assign 之前没有 use 该变量——
   把声明下沉到该 assign 的位置，合并成带初始化的声明：

   ▸ 之前：`int x; ...; x = 0;`
   ▸ 之后：`int x = 0;`（在 assign 的原位置）

   ▸ 如果是 `use(x); x = 0;`——assign 之前有 use——不下沉，
     声明留在 block 开头
   ▸ 如果是 `x = 0; use(x);`——assign 之前无 use——下沉成
     `int x = 0; use(x);`

   ── 状态 ──

   VarInfo 记录每个变量的出现信息：
   ▸ :paths —— 所有出现位置的 block 路径（#{路径 ...}）
   ▸ :attrs —— 变量的 attrs（从第一次出现的节点继承）

   ── freevars 过滤 ──

   参数和 freevars（全局变量）排除——不声明。"
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 状态记录
;; ═══════════════════════════════════════════

(defrecord VarInfo [paths attrs])

;; ═══════════════════════════════════════════
;; 第一遍：收集出现位置——thread 状态
;; ═══════════════════════════════════════════

(defn- record-occurrence
  [state var-name block-path attrs]
  (if-let [info (get state var-name)]
    (assoc state var-name (update info :paths conj block-path))
    (assoc state var-name (->VarInfo #{block-path} attrs))))

(defn- walk-collect
  "收集变量的出现位置。返回新 state。

   block-path 是从函数体到当前 block 的祖先链（含当前 block）。"
  [node block-path state]
  (case (ast/kind node)
    :variable
    (record-occurrence state (:name node) block-path (ast/attrs node))

    :assign
    (let [lhs   (:var node)
          state (record-occurrence state (:name lhs) block-path (ast/attrs lhs))]
      (walk-collect (:val node) block-path state))

    :var-decl
    (let [state (record-occurrence state (:name node) block-path (ast/attrs node))]
      (if-let [val (:val node)]
        (walk-collect val block-path state)
        state))

    :block
    (let [block-path' (conj block-path node)
          state       (reduce (fn [s stmt] (walk-collect stmt block-path' s))
                              state
                              (:stmts node))]
      (if-let [ret (:ret node)]
        (walk-collect ret block-path' state)
        state))

    :if
    (let [state (walk-collect (:test node) block-path state)
          state (walk-collect (:then node) block-path state)]
      (if-let [else (:else node)]
        (walk-collect else block-path state)
        state))

    :while
    (let [state (walk-collect (:test node) block-path state)]
      (walk-collect (:body node) block-path state))

    :throw
    (walk-collect (:expr node) block-path state)

    (reduce (fn [s child] (walk-collect child block-path s))
            state
            (ast/children node))))

;; ═══════════════════════════════════════════
;; LCA
;; ═══════════════════════════════════════════

(defn- all-identical?
  [xs]
  (or (empty? xs)
      (every? #(identical? (first xs) %) xs)))

(defn- lca-of-paths
  [paths]
  (let [paths    (vec paths)
        shortest (apply min (map count paths))]
    (loop [i 0]
      (if (and (< i shortest)
               (all-identical? (map #(nth % i) paths)))
        (recur (inc i))
        (when (pos? i) (nth (first paths) (dec i)))))))

;; ═══════════════════════════════════════════
;; 自赋值判定
;; ═══════════════════════════════════════════

(defn- self-assign?
  "`x = x`——destruct 下推 phi 的副产物——删除安全。"
  [stmt]
  (and (= :assign (ast/kind stmt))
       (let [lhs (:var stmt)
             rhs (:val stmt)]
         (and (= :variable (ast/kind lhs))
              (= :variable (ast/kind rhs))
              (= (:name lhs) (:name rhs))))))

;; ═══════════════════════════════════════════
;; use 收集——用于下沉判定
;; ═══════════════════════════════════════════

(defn- collect-var-uses
  "节点中所有 variable use 的 var-name 集合。

   assign 只收集 rhs——lhs 是 def 不是 use。
   block / if / while / throw 递归所有子节点。"
  [node]
  (case (ast/kind node)
    :variable #{(:name node)}
    :assign   (collect-var-uses (:val node))
    :block    (let [stmts-uses (reduce (fn [acc stmt]
                                         (into acc (collect-var-uses stmt)))
                                       #{}
                                       (:stmts node))]
                (if-let [ret (:ret node)]
                  (into stmts-uses (collect-var-uses ret))
                  stmts-uses))
    :if       (let [uses (into (collect-var-uses (:test node))
                               (collect-var-uses (:then node)))]
                (if-let [else (:else node)]
                  (into uses (collect-var-uses else))
                  uses))
    :while    (into (collect-var-uses (:test node))
                    (collect-var-uses (:body node)))
    :throw    (collect-var-uses (:expr node))
    (reduce (fn [acc child] (into acc (collect-var-uses child)))
            #{}
            (ast/children node))))

;; ═══════════════════════════════════════════
;; 下沉判定
;; ═══════════════════════════════════════════

(defn- first-assign-index
  "stmts 里第一次 assign var-name 的 stmt 索引。没有则 nil。"
  [stmts var-name]
  (first (keep-indexed
           (fn [idx stmt]
             (when (and (= :assign (ast/kind stmt))
                        (= :variable (ast/kind (:var stmt)))
                        (= var-name (:name (:var stmt))))
               idx))
           stmts)))

(defn- can-sink?
  "从 stmts 开头到 idx 之间没有 use var-name——声明可以下沉到
   idx 位置的 assign。"
  [stmts idx var-name]
  (not-any? (fn [stmt] (contains? (collect-var-uses stmt) var-name))
            (take idx stmts)))

(defn- sink-decl
  "尝试把 decl 下沉到 stmts 里第一次 assign 的位置。

   返回 [stmts' sunk?]——sunk? true 表示下沉成功（stmts' 里
   assign 被替换成带初始化的 decl）。"
  [stmts {:keys [var-name attrs]}]
  (let [idx (first-assign-index stmts var-name)]
    (if (and idx (can-sink? stmts idx var-name))
      (let [assign-stmt (nth stmts idx)
            decl-stmt   (ast/->VarDecl var-name
                                       (:val assign-stmt)
                                       nil attrs nil)]
        [(assoc stmts idx decl-stmt) true])
      [stmts false])))

;; ═══════════════════════════════════════════
;; 第二遍：插入声明 + 删自赋值 + 下沉
;; ═══════════════════════════════════════════

(declare insert-decls-node)

(defn- insert-decls-block
  [block infos]
  (let [my-decls (sort-by (comp str :var-name)
                          (filter #(identical? block (:top-block %)) infos))
        stmts0   (->> (:stmts block)
                      (mapv #(insert-decls-node % infos))
                      (remove self-assign?)
                      vec)
        ;; 逐个尝试下沉
        [stmts' remaining-decls]
        (reduce (fn [[stmts acc-decls] decl-info]
                  (let [[stmts' sunk?] (sink-decl stmts decl-info)]
                    (if sunk?
                      [stmts' acc-decls]
                      [stmts' (conj acc-decls decl-info)])))
                [stmts0 []]
                my-decls)
        ;; 未下沉的 decl 放开头
        top-decls (mapv (fn [{:keys [var-name attrs]}]
                          (ast/->VarDecl var-name nil nil attrs nil))
                        remaining-decls)]
    (-> block
        (assoc :stmts (into top-decls stmts'))
        (update :ret #(when % (insert-decls-node % infos))))))

(defn- insert-decls-node
  [node infos]
  (case (ast/kind node)
    :block (insert-decls-block node infos)
    :if    (-> node
               (update :test #(insert-decls-node % infos))
               (update :then #(insert-decls-node % infos))
               (update :else #(when % (insert-decls-node % infos))))
    :while (-> node
               (update :test #(insert-decls-node % infos))
               (update :body #(insert-decls-node % infos)))
    node))

;; ═══════════════════════════════════════════
;; polyfill-function
;; ═══════════════════════════════════════════

(defn polyfill-function
  "为 function/method 节点重建 var-decl。

   freevars —— #{var-name}——函数中 use 但不在函数内 def 的变量
                （来自 free-vars/analyze-function）。

   参数和 freevars 排除——不声明。局部变量在 LCA block 插入
   VarDecl——能下沉的合并到第一次 assign 的位置——自赋值
   `x = x` 删除。"
  [node freevars]
  (let [params (set (map :name (:params node)))
        state  (walk-collect (:body node) [] {})
        infos  (->> state
                    (remove (fn [[var-name _]]
                              (or (contains? params var-name)
                                  (contains? freevars var-name))))
                    (mapv (fn [[var-name ^VarInfo info]]
                            {:var-name  var-name
                             :attrs     (:attrs info)
                             :top-block (lca-of-paths (:paths info))})))
        body'  (insert-decls-node (:body node) infos)]
    (assoc node :body body')))
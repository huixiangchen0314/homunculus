(ns top.kzre.homunculus.frontend.shader.pass.loopfor.rotate
  "循环旋转——脑内记账。

   GLSL 没有 do-while，不能真的做 loop rotation。识别自然循环时，
   按旋转后的概念视图分析：

     while (flag) {                      旋转后概念视图：
       if (cond) { exit; flag=false; }     guard:  test flag
       else { loop; update; flag=true; }   body:   loop; update
     }                                    latch:  test cond

   rotate 只做分析——输出结构化的分析结果给 core。所有函数是纯函数，
   不修改 AST。

   ── 检查顺序 ──

   归纳变量识别必须在 cond 捕获检查之前——cond 里的归纳变量
   虽然被 body 定义，但它受 for 的 update 控制，不算\"非捕获\"。
   先找出归纳变量，再从 cond 的变量集合里排除它们，剩余的才
   需要是捕获。

   1. test 是单一 bool 变量（flag）
   2. body 的 stmts 恰好是一个 if
   3. if 的 then/else 能分类为 exit（flag=false）/ loop（flag=true）
   4. loop 分支里至少一处对 cond 变量的仿射 update
   5. 所有 update 之间互不依赖——各自的 rhs 不 use 其他 update 的 lhs
   6. 每个归纳变量在 while body 全体里只被自己的 update 定义一次
   7. cond 里除归纳变量外的变量都是 while body 的捕获——不在 body 内 def
   8. primary update 之后的 stmt（排除 flag、other updates）不 use
      primary lhs"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.frontend.shader.ast :as ast]))

;; ═══════════════════════════════════════════
;; 表达式辅助
;; ═══════════════════════════════════════════

(defn node-uses
  "节点中所有 variable use 的 name 集合。"
  [node]
  (if (nil? node)
    #{}
    (if (= :variable (ast/kind node))
      #{(:name node)}
      (reduce (fn [acc child] (into acc (node-uses child)))
              #{} (ast/children node)))))

(defn expr-vars
  "表达式里 variable use 的 name 集合。"
  [expr]
  (node-uses expr))

(defn stmt-uses
  "stmt 中所有 variable use 的 name 集合。
   assign 的 lhs 是 def——只收集 rhs。"
  [stmt]
  (if (nil? stmt)
    #{}
    (case (ast/kind stmt)
      :assign   (node-uses (:rhs stmt))
      :var-decl (node-uses (:init stmt))
      (node-uses stmt))))

(defn- literal-bool?
  "expr 是 true/false 字面量时返回对应 bool，否则 nil。"
  [expr]
  (when (and expr (= :literal (ast/kind expr)))
    (let [v (:val expr)]
      (cond
        (true? v)  true
        (false? v) false
        :else      nil))))

;; ═══════════════════════════════════════════
;; stmt 辅助
;; ═══════════════════════════════════════════

(defn- stmt-defs
  [stmt]
  (cond
    (nil? stmt) #{}
    (= :assign (ast/kind stmt))
    (let [lhs (:lhs stmt)]
      (if (= :variable (ast/kind lhs))
        #{(:name lhs)}
        #{}))
    (= :var-decl (ast/kind stmt))
    #{(:name stmt)}
    :else #{}))

(defn- body-def-names
  [stmts]
  (reduce (fn [acc stmt] (into acc (stmt-defs stmt))) #{} stmts))

;; ═══════════════════════════════════════════
;; 仿射判定
;; ═══════════════════════════════════════════

(defn- affine-in-var?
  "expr 是否是 var 的仿射表达式：var + c / var - c / c + var /
   var * c / var / c / c * var，其中 c 不含 var。`i = j` 不算——
   那是别名不是步进。"
  [expr var]
  (when (and expr (= :binary-op (ast/kind expr)))
    (let [op         (:op expr)
          left       (:left expr)
          right      (:right expr)
          uses-left  (contains? (expr-vars left) var)
          uses-right (contains? (expr-vars right) var)]
      (and (contains? #{:+ :- :* :/} op)
           (or (and uses-left (not uses-right))
               (and uses-right (not uses-left)))))))

;; ═══════════════════════════════════════════
;; 分支分类
;; ═══════════════════════════════════════════

(defn- classify-branch
  "对 if 的一个分支的 stmts 分类。

   在 stmts 里找对 flag 的 assign：恰好一个、值是 true → :loop；
   值是 false → :exit；其他（0 个、多个、非字面量）→ nil。"
  [branch-stmts flag]
  (let [flag-assigns
        (keep-indexed
          (fn [i stmt]
            (when (and (= :assign (ast/kind stmt))
                       (= :variable (ast/kind (:lhs stmt)))
                       (= flag (:name (:lhs stmt))))
              [i stmt]))
          branch-stmts)]
    (when (= 1 (count flag-assigns))
      (let [[idx stmt] (first flag-assigns)
            v (literal-bool? (:rhs stmt))]
        (when (some? v)
          {:kind           (if v :loop :exit)
           :flag-assign-idx idx})))))

;; ═══════════════════════════════════════════
;; 归纳变量 update 识别
;; ═══════════════════════════════════════════

(defn- find-induction-updates
  "在 loop-stmts 里找对 cond-vars 中某个变量的仿射 update。
   返回 [{:var v :update stmt} ...]——按 stmt 顺序。"
  [loop-stmts cond-vars]
  (vec (keep (fn [stmt]
               (when (and (= :assign (ast/kind stmt))
                          (= :variable (ast/kind (:lhs stmt)))
                          (contains? cond-vars (:name (:lhs stmt)))
                          (affine-in-var? (:rhs stmt) (:name (:lhs stmt))))
                 {:var    (:name (:lhs stmt))
                  :update stmt}))
             loop-stmts)))

(defn- updates-independent?
  "多个 update 之间互不依赖——每个 update 的 rhs 不 use 其他 update 的 lhs。"
  [updates]
  (let [lhs-set (set (map :var updates))]
    (every? (fn [{:keys [var update]}]
              (let [others (disj lhs-set var)]
                (empty? (set/intersection (node-uses (:rhs update)) others))))
            updates)))

(defn- induction-vars-single-def?
  "每个归纳变量在 all-body-stmts 里只定义一次（就是自己的 update）。"
  [updates all-body-stmts]
  (every? (fn [{:keys [var update]}]
            (let [defs (filter (fn [stmt]
                                 (and (= :assign (ast/kind stmt))
                                      (= :variable (ast/kind (:lhs stmt)))
                                      (= var (:name (:lhs stmt)))))
                               all-body-stmts)]
              (and (= 1 (count defs))
                   (identical? (first defs) update))))
          updates))

;; ═══════════════════════════════════════════
;; cond 捕获检查——排除归纳变量
;; ═══════════════════════════════════════════

(defn- non-induction-cond-vars
  "cond 里除了归纳变量之外的变量——这些必须是捕获。"
  [cond-expr updates]
  (let [cond-vars (expr-vars cond-expr)
        lhs-set   (set (map :var updates))]
    (set/difference cond-vars lhs-set)))

(defn- captured-vars?
  "vars 里所有变量都不在 body-def-names 里。"
  [vars body-def-names]
  (every? #(not (contains? body-def-names %)) vars))

;; ═══════════════════════════════════════════
;; primary update 之后的 stmt 检查
;; ═══════════════════════════════════════════

(defn- stmt-index
  [stmts stmt]
  (first (keep-indexed (fn [i s] (when (identical? s stmt) i)) stmts)))

(defn- safe-after-primary?
  "primary update 之后（排除 flag assign 和 other updates）的 stmt
   不 use primary lhs——保证 for 里 primary update 移到最后后语义不变。"
  [loop-st primary-update primary-lhs other-updates flag-assign-stmt]
  (let [idx       (stmt-index loop-st primary-update)
        other-set (set (map :update other-updates))
        after     (drop (inc idx) loop-st)
        relevant  (remove (fn [stmt]
                            (or (identical? stmt flag-assign-stmt)
                                (contains? other-set stmt)))
                          after)]
    (not-any? (fn [stmt] (contains? (stmt-uses stmt) primary-lhs)) relevant)))

;; ═══════════════════════════════════════════
;; 辅助
;; ═══════════════════════════════════════════

(defn- remove-nth [coll idx]
  (vec (concat (take idx coll) (drop (inc idx) coll))))

(defn- remove-identity [coll x]
  (vec (remove #(identical? % x) coll)))

;; ═══════════════════════════════════════════
;; 主分析
;; ═══════════════════════════════════════════

(defn analyze-while
  "对 while 节点做识别。

   返回：
   ▸ {:kind :natural :flag :guard :exit-stmts :loop-stmts :update
      :other-updates :induction-var :other-induction-vars :branch-kind}
   ▸ {:kind :degraded :reason kw}"
  [while-node]
  (if (not= :while (ast/kind while-node))
    {:kind :degraded :reason :not-while}
    (let [test       (:test while-node)
          body       (:body while-node)
          body-stmts (:stmts body)]
      (cond
        ;; 检查 1——test 是单一 bool 变量
        (not (= :variable (ast/kind test)))
        {:kind :degraded :reason :test-not-var}

        ;; 检查 2——body 恰好一个 if
        (not (and (= 1 (count body-stmts))
                  (= :if (ast/kind (first body-stmts)))))
        {:kind :degraded :reason :body-not-single-if}

        :else
        (let [flag     (:name test)
              if-node  (first body-stmts)
              then-st  (:stmts (:then if-node))
              else-node (:else if-node)
              else-st  (when else-node (:stmts else-node))
              then-cls (classify-branch then-st flag)
              else-cls (when else-st (classify-branch else-st flag))]
          (cond
            ;; 检查 3——分支分类
            (or (nil? then-cls) (nil? else-cls))
            {:kind :degraded :reason :branch-classify-failed}

            (= (:kind then-cls) (:kind else-cls))
            {:kind :degraded :reason :same-branch-kind}

            :else
            (let [[exit-cls exit-st loop-cls loop-st]
                  (if (= :exit (:kind then-cls))
                    [then-cls then-st else-cls else-st]
                    [else-cls else-st then-cls then-st])
                  cond-expr      (:test if-node)
                  all-body-stmts (concat then-st else-st)
                  body-def-names (body-def-names all-body-stmts)
                  updates        (find-induction-updates loop-st (expr-vars cond-expr))]

              ;; 检查 4——至少一个仿射 update
              (if (empty? updates)
                {:kind :degraded :reason :no-update}

                ;; 检查 5——updates 互不依赖
                (if (not (updates-independent? updates))
                  {:kind :degraded :reason :dependent-updates}

                  ;; 检查 6——每个归纳变量单 def
                  (if (not (induction-vars-single-def? updates all-body-stmts))
                    {:kind :degraded :reason :multi-def}

                    ;; 检查 7——cond 里除归纳变量外的变量都是捕获
                    (let [non-induction-vars (non-induction-cond-vars cond-expr updates)]
                      (if (not (captured-vars? non-induction-vars body-def-names))
                        {:kind :degraded :reason :cond-has-local-vars}

                        ;; 检查 8——primary update 之后不 use primary lhs
                        (let [primary          (first updates)
                              other-updates    (vec (rest updates))
                              flag-assign-idx  (:flag-assign-idx loop-cls)
                              flag-assign-stmt (nth loop-st flag-assign-idx)]
                          (if (not (safe-after-primary?
                                     loop-st (:update primary) (:var primary)
                                     other-updates flag-assign-stmt))
                            {:kind :degraded :reason :unsafe-after-primary}
                            {:kind          :natural
                             :flag          flag
                             :guard         cond-expr
                             :exit-stmts    (remove-nth exit-st (:flag-assign-idx exit-cls))
                             :loop-stmts    (remove-identity
                                              (remove-nth loop-st flag-assign-idx)
                                              (:update primary))
                             :update        (:update primary)
                             :other-updates other-updates
                             :induction-var (:var primary)
                             :other-induction-vars (mapv :var other-updates)
                             :branch-kind   (if (= :exit (:kind then-cls))
                                              :exit-then
                                              :exit-else)}))))))))))))))
(ns top.kzre.homunculus.core.irstmt.statementize
  (:require
   [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defrecord Env [])
(defn make-env [] (->Env))

(defmulti statementize-node
          (fn [node _env] (ast/kind node)))

(defmethod statementize-node :default [node env] [node env])

(defn lift-block
  "提升 Block：保留 stmts，用 expr-fn 基于 ret 生成新表达式节点作为新 Block 的 ret。"
  [block expr-fn]
  (let [stmts (:stmts block)
        ret (:ret block)
        [stmts' ret']
        (if (when ret (= :assign (ast/kind ret)))
          [(conj stmts ret)
           (:var ret)]
          [stmts ret])
        new-expr (expr-fn ret')]
    (ast/->Block stmts' new-expr (ast/attrs block) (ast/node-meta block))))


(defmethod statementize-node :var-decl [node env]
  (if-let [init-expr (:val node)]
    (if (= :block (ast/kind init-expr))
      (let [new-node
            (lift-block init-expr #(assoc node :val %))]
        [new-node env])
      [node env])
    [node env]))

(defmethod statementize-node :assign [node env]
  (if-let [rhs (:val node)]
    (if (= :block (ast/kind rhs))
      (let [new-node
            (lift-block rhs #(assoc node :val %))]
        [new-node env])
      [node env])
    [node env]))

(defmethod statementize-node :if [node env]
  (let [test (:test node)]
    (if (= :block (ast/kind test))
      (let [new-node
            (lift-block test #(assoc node :test %))]
        [new-node env])
      [node env])))

(defmethod statementize-node :while [node env]
  (let [test (:test node)]
    (if (= :block (ast/kind test))
      (let [new-node (lift-block test #(assoc node :test %))]
        [new-node env])
      [node env])))

(defmethod statementize-node :call [node env]
  (let [args (:args node)
        collected (reduce (fn [[stmts new-args] arg]
                            (if (= :block (ast/kind arg))
                              [(into stmts (:stmts arg)) (conj new-args (:ret arg))]
                              [stmts (conj new-args arg)]))
                          [[] []]
                          args)]
    (if (seq (first collected))
      ;; 存在需要前置的语句，生成包含语句和新调用的 Block
      [(ast/->Block (first collected) (assoc node :args (second collected))
                    (ast/attrs node)
                    (ast/node-meta node)) env]
      [node env])))

(defmethod statementize-node :aget [node env]
  (let [target (:target node)
        idx    (:idx node)
        [stmts1 new-target] (if (= :block (ast/kind target))
                              [(:stmts target) (:ret target)]
                              [[] target])
        [stmts2 new-idx]    (if (= :block (ast/kind idx))
                              [(:stmts idx) (:ret idx)]
                              [[] idx])
        all-stmts (into stmts1 stmts2)]
    (if (seq all-stmts)
      [(ast/->Block all-stmts
                    (assoc node :target new-target :idx new-idx)
                    (ast/attrs node)
                    (ast/node-meta node))
       env]
      [node env])))

(defmethod statementize-node :aset [node env]
  (let [target (:target node)
        idx    (:idx node)
        val    (:val node)
        [stmts1 new-target] (if (= :block (ast/kind target))
                              [(:stmts target) (:ret target)]
                              [[] target])
        [stmts2 new-idx]    (if (= :block (ast/kind idx))
                              [(:stmts idx) (:ret idx)]
                              [[] idx])
        [stmts3 new-val]    (if (= :block (ast/kind val))
                              [(:stmts val) (:ret val)]
                              [[] val])
        all-stmts (into (into stmts1 stmts2) stmts3)]
    (if (seq all-stmts)
      [(ast/->Block all-stmts
                    (assoc node :target new-target :idx new-idx :val new-val)
                    (ast/attrs node)
                    (ast/node-meta node))
       env]
      [node env])))

(defmethod statementize-node :block [node env]
  ;; 子节点已由 walk 递归处理，这里只处理当前 Block 层
  (let [;; 处理 stmts：展开内部 Block
        new-stmts
        (vec
          (mapcat (fn [child]
                    (if (= :block (ast/kind child))
                      (let [inner-stmts (:stmts child)
                            inner-ret   (:ret child)]
                        ;; 将 inner-stmts 加入，如果 inner-ret 非 nil 则作为表达式语句追加
                        (if inner-ret
                          (conj (vec inner-stmts) inner-ret)
                          inner-stmts))
                      [child]))
                  (:stmts node)))
        ;; 处理 ret：如果 ret 是 Block，则合并其 stmts，并用其 ret 替代
        [final-stmts new-ret]
        (if-let [ret (:ret node)]
          (if (= :block (ast/kind ret))
            (let [inner-stmts (:stmts ret)
                  inner-ret   (:ret ret)]
              [(into new-stmts inner-stmts) inner-ret])
            [new-stmts ret])
          [new-stmts nil])]
    [(ast/->Block (vec final-stmts) new-ret (ast/attrs node) (ast/node-meta node)) env]))

;; ── 递归 walk 定义 ──
(declare walk)

(defn stmt-fn [node env]
  (let [[new-node env1] (walk node env)]
    (statementize-node new-node env1)))

(defn walk [node env]
  (when node
    (ast/reduce-children node stmt-fn env)))

(defn statementize-nodes [nodes]
  (let [processed (mapv #(first (stmt-fn % (make-env))) nodes)]
    (vec (mapcat (fn [node]
                   (if (= :block (ast/kind node))
                     (:stmts node)
                     [node]))
                 processed))))
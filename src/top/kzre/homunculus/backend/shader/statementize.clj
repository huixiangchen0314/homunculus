(ns top.kzre.homunculus.backend.shader.statementize
  "将 ShaderAST 函数体转换为语句序列，显式提取返回值，并清理冗余 Block 层级。"
  (:require
    [top.kzre.homunculus.backend.shader.ast :as ast]))

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
        new-expr (expr-fn ret)]
    (ast/->Block stmts new-expr nil)))


(defmethod statementize-node :var-decl [node env]
  (if-let [init-expr (:init node)]
    (if (= :block (ast/kind init-expr))
      (let [new-node
            (lift-block init-expr #(assoc node :init %))]
        [new-node env])
      [node env])
    [node env]))

(defmethod statementize-node :assign [node env]
 (if-let [rhs (:rhs node)]
   (if (= :block (ast/kind rhs))
     (let [new-node
           (lift-block rhs #(assoc node :rhs %))]
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
      [(ast/->Block (first collected) (assoc node :args (second collected)) (ast/node-meta node)) env]
      [node env])))

(defmethod statementize-node :constructor [node env]
  (let [args (:args node)
        collected (reduce (fn [[stmts new-args] arg]
                            (if (= :block (ast/kind arg))
                              [(into stmts (:stmts arg)) (conj new-args (:ret arg))]
                              [stmts (conj new-args arg)]))
                          [[] []]
                          args)]
    (if (seq (first collected))
      [(ast/->Block (first collected) (assoc node :args (second collected)) (ast/node-meta node)) env]
      [node env])))

(defmethod statementize-node :array-index [node env]
  (let [target (:target node)
        index  (:index node)
        ;; 分别处理 target 和 index 可能为 Block 的情形
        [stmts1 new-target] (if (= :block (ast/kind target))
                              [(:stmts target) (:ret target)]
                              [[] target])
        [stmts2 new-index]  (if (= :block (ast/kind index))
                              [(:stmts index) (:ret index)]
                              [[] index])
        all-stmts (into stmts1 stmts2)]
    (if (seq all-stmts)
      ;; 存在需要前置的语句，将它们与新的 array-index 包装成 Block
      [(ast/->Block all-stmts (assoc node :target new-target :index new-index) (ast/node-meta node)) env]
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
    [(ast/->Block (vec final-stmts) new-ret (ast/node-meta node)) env]))

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
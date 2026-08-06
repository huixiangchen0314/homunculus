(ns top.kzre.homunculus.backend.shader.vec-assign
  "Statementize 后展开数组整体赋值为临时变量 + 逐元素赋值。"
  (:require
   [top.kzre.homunculus.backend.shader.ast :as ast]
   [top.kzre.homunculus.core.types.type :as ty]))

(defn- expand-assign [assign]
  (let [meta     (:meta assign)
        vec-type (:vec-type meta)
        size     (ty/vec-size vec-type)
        lhs      (:lhs assign)
        rhs      (:rhs assign)]
    (mapv (fn [i]
            (let [idx-lit (ast/->Literal i nil)
                  lhs-idx (ast/->ArrayIndex lhs idx-lit nil)
                  rhs-idx (ast/->ArrayIndex rhs idx-lit nil)]
              (ast/->Assign lhs-idx rhs-idx nil)))
          (range size))))

(defn- expand-node [node]
  (case (ast/kind node)
    :block
    (let [new-stmts (mapcat (fn [stmt]
                              (if (and (= :assign (ast/kind stmt))
                                       (:vec-assign? (ast/node-meta stmt)))
                                (expand-assign stmt)
                                [(expand-node stmt)]))
                            (:stmts node))
          new-ret   (when-let [ret (:ret node)] (expand-node ret))]
      (ast/->Block (vec new-stmts) new-ret (ast/node-meta node)))
    ;; 其他节点递归子节点
    (first
      (ast/reduce-children node (fn [child _] [(expand-node child) nil]) nil))))

(defn expand-vec-assigns [nodes]
  (mapv expand-node nodes))
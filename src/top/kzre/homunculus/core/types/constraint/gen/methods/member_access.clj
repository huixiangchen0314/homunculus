(ns top.kzre.homunculus.core.types.constraint.gen.methods.member-access
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))


;; TODO 添加成员访问的新约束.
(defmethod gen/cg-node-raw :member-access [node context]
  ;; 推导 target，获得四元组
  (let [[_target-tv target-node target-constr target-ctx] (gen/cg-node-raw (n/access-target node) context)
        args (n/access-args node)
        ;; 顺序处理每个 arg，传递上下文
        [arg-results final-ctx]
        (reduce (fn [[results ctx] arg]
                  (let [[arg-tv arg-node arg-constr arg-ctx] (gen/cg-node-raw arg ctx)]
                    [(conj results {:tv arg-tv :node arg-node :constr arg-constr})
                     arg-ctx]))
                [[] target-ctx]
                args)
        arg-tys (mapv :tv arg-results)
        arg-nodes (mapv :node arg-results)
        arg-constr (mapcat :constr arg-results)
        ;; 成员访问整体类型：新鲜类型变量（因为编译器不知道成员类型）
        tv (gen/fresh-tvar)
        new-node (n/make-member-access target-node
                                       (n/access-member node)
                                       arg-nodes
                                       (n/node-meta node) (n/parent node))]
    ;; 返回四元组：类型、新节点、约束、最终上下文
    [tv (ty/set-type! new-node tv) (concat target-constr arg-constr) final-ctx]))
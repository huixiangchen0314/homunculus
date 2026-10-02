(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.vector
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod gen/gen-node* :vector [current-node env]
  (let [items (n/vector-items current-node)
        ;; 顺序处理每个元素，累积节点、类型、约束、环境
        [item-nodes item-types collected-constraints final-env]
        (reduce
          (fn [[nodes types collected current-env] item]
            (let [{:keys [type node constraints env]} (gen/gen-node* item current-env)]
              [(conj nodes node)
               (conj types type)
               (into collected constraints)
               env]))
          [[] [] [] env]
          items)
        [vec-type extra-constraints]
        (if (p/use-hetero-vec? env)
          ;; 异构向量：保留所有元素类型
          [(ty/make-hetero-vec item-types) []]
          ;; 同构向量：所有元素类型必须一致，引入公共元素类型变量
          (let [elem-tv      (gen/fresh-tvar)
                elem-constrs (mapv (fn [item-type]
                                     (cons/make-cequal elem-tv item-type))
                                   item-types)]
            [(ty/make-tvec elem-tv (ty/make-tvalue (count items)))
             elem-constrs]))
        new-node (n/make-vector (vec item-nodes)
                                (n/attrs current-node)
                                (n/node-meta current-node))]
    {:type        vec-type
     :node        (ty/set-type! new-node vec-type)
     :constraints (into collected-constraints extra-constraints)
     :env         final-env}))
(ns top.kzre.homunculus.core.ir2.pass.infer.methods.vector
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.model :as t]
    [top.kzre.homunculus.core.ir2.pass.type :as type]))

(defmethod infer/infer-node* :vector [node env]
  (let [items (n/vector-items node)
        ;; 顺序推导每个元素，累积新节点、类型以及环境
        [item-nodes item-types final-env]
        (reduce (fn [[nodes types current-env] item]
                  (let [[type new-item new-env] (infer/infer-node* item current-env)]
                    [(conj nodes new-item) (conj types type) new-env]))
                [[] [] env]
                items)
        ;; 根据后端配置决定向量类型
        vec-type (if (p/use-hetero-vec? env)
                   ;; 支持异构向量：保留每个元素的独立类型
                   (t/->THeteroVec item-types)
                   ;; 不支持异构：构造同构向量，元素类型采用第一个元素类型
                   (let [elem-type (first item-types)]   ; 若无元素则为 nil
                     (t/->TVec elem-type (count item-types))))
        new-node   (n/vector-with-items node item-nodes)
        typed-node (type/set-type! new-node vec-type)]
    (if (every? some? item-types)
      (infer/success vec-type typed-node final-env)
      (infer/nothing typed-node final-env))))
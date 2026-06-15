(ns top.kzre.homunculus.core.types.infer.methods.vector
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.types.model :as t]))

(defmethod infer/local-infer :vector [node context]
  (let [items (n/vector-items node)
        ;; 急切推导所有元素，得到 [type, new-node] 对
        results (mapv #(infer/local-infer % context) items)
        item-tys   (mapv first results)
        item-nodes (mapv second results)
        ;; 构造异构向量类型：保留每个元素的独立类型
        vec-type   (t/->THeteroVec item-tys)
        ;; 使用工具函数重建节点（假设已存在 vector-with-items，若没有则用 assoc）
        new-node   (n/vector-with-items node item-nodes)]
    ;; 只有当所有元素都成功推导（无 nil 类型）时，才认为向量类型有效
    (if (every? some? item-tys)
      (infer/success vec-type (type/set-type! new-node vec-type))
      (infer/nothing (type/set-type! new-node vec-type)))))
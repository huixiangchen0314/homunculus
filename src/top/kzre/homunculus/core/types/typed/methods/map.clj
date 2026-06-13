(ns top.kzre.homunculus.core.types.typed.methods.map
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.node :as node]))

(defmethod infer/infer :map [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [kvs (node/map-kvs node)
          pairs (partition 2 kvs)
          ;; 推断键值对并收集结果
          results (mapcat (fn [[k v]]
                            (let [[kt kn] (infer/infer k context)
                                  [vt vn] (infer/infer v context)]
                              [kn vn]))
                          pairs)
          ;; 形状固定为 MapShape
          shape (t/->MapShape)
          ;; 键类型和值类型：取第一个键值对的类型，若无则 TVar
          first-pair (first pairs)
          kt (if first-pair (first (infer/infer (first first-pair) context)) (t/->TVar (gensym "k")))
          vt (if first-pair (first (infer/infer (second first-pair) context)) (t/->TVar (gensym "v")))
          ;; 构造 TContainer
          ty (t/->TContainer :map [kt vt] shape)
          new-node (type/set-type! (assoc node :kvs (vec results)) ty)]
      [ty new-node {}])))
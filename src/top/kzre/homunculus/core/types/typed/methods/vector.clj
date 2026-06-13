(ns top.kzre.homunculus.core.types.typed.methods.vector
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.node :as node]))

(defmethod infer/infer :vector [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [items (node/vec-items node)
          ;; 推断每个元素
          results (mapv #(infer/infer % context) items)
          item-types (mapv first results)
          item-nodes (mapv second results)
          ;; 判断是否所有元素都是字面量，从而决定形状
          all-literal? (every? #(= :literal (node/kind %)) item-nodes)
          shape (if all-literal?
                  (t/->FixedLength (count items))
                  (t/->VariableLength))
          ;; 元素类型：取第一个元素的类型，否则泛型变量
          elem-ty (if (seq item-types)
                    (first item-types)
                    (t/->TVar (gensym "e")))
          ;; 构造完整的容器类型
          ty (t/->TContainer :vector elem-ty shape)
          ;; 更新节点
          new-node (type/set-type! (assoc node :items item-nodes) ty)]
      [ty new-node {}])))
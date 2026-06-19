(ns top.kzre.homunculus.core.types.infer.methods.record
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod infer/local-infer :record [node context]
  ;; 处理每个字段，顺序推导初始值表达式，累积上下文
  (let [fields (n/record-fields node)
        [new-fields final-ctx]
        (reduce (fn [[flds ctx] field]
                  (if-let [init (n/field-init field)]
                    (let [[_ new-init new-ctx] (infer/local-infer init ctx)]
                      [(conj flds (n/field-with-init field new-init)) new-ctx])
                    [(conj flds field) ctx]))
                [[] context]
                fields)
        ;; 重建 record 节点
        new-node (n/record-with-fields node new-fields)
        ;; 将记录类型符号写入已知类型集合
        record-name (n/record-name node)
        record-type (ty/make-tcon record-name)
        new-ctx (infer/add-known-type final-ctx record-name)]
    ;; 返回记录类型、更新后的节点、更新了已知类型集合的上下文
    (infer/success record-type new-node new-ctx)))
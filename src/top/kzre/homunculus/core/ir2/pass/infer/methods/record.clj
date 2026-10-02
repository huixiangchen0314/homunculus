(ns top.kzre.homunculus.core.ir2.pass.infer.methods.record
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod infer/infer-node* :record [node env]
  ;; 处理每个字段，顺序推导初始值表达式，累积环境
  (let [fields (n/record-fields node)
        [new-fields final-env]
        (reduce (fn [[fields current-env] field]
                  (if-let [init (n/field-init field)]
                    (let [[_ new-init new-env] (infer/infer-node* init current-env)]
                      [(conj fields (n/field-with-init field new-init)) new-env])
                    [(conj fields field) current-env]))
                [[] env]
                fields)
        ;; 重建 record 节点
        new-node    (n/record-with-fields node new-fields)
        record-name (n/record-name node)
        record-type (ty/make-tcon record-name)]
    ;; 推断类型为记录类型本身
    (infer/success record-type new-node final-env)))
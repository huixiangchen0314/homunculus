(ns top.kzre.homunculus.core.types.constraint.gen.methods.record
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.type :as ty]))


(defmethod gen/cg-node-raw :record [node context]
  (let [fields     (n/record-fields node)
        env         (:env context)
        symbol-table (:symbol-table context)
        ;; 处理每个字段：有初始值就递归生成，并收集约束
        results    (mapv (fn [field]
                           ;; 先从
                           (if-let [init (n/field-init field)]
                             (let [[_ new-init constr] (gen/cg-node-raw init context)]
                               [(n/field-with-init field new-init) constr])
                             [field nil]))
                         fields)
        new-fields (mapv first results)          ;; 更新后的字段列表
        constrs    (mapcat second results)       ;; 所有字段生成的约束
        tv         (gen/fresh-tvar)              ;; 记录类型对应的类型变量
        new-node   (-> node
                       (n/record-with-fields new-fields)
                       (ty/set-type! tv))]       ;; 将类型变量赋予新节点
    [tv new-node constrs]))
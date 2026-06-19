(ns top.kzre.homunculus.core.types.constraint.gen.methods.record
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.constraint :as c]
    [top.kzre.homunculus.core.types.constraint.utils :as u]
    [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :record [node context]
  (let [fields       (n/record-fields node)
        record-name  (n/record-name node)
        known-types  (u/known-types context)
        ;; 顺序处理字段，累积上下文
        [new-fields constrs final-ctx]
        (reduce
          (fn [[flds constrs ctx] field]
            (let [init-expr (n/field-init field)
                  ;; 用统一入口获取字段声明类型
                  declared  (ty/meta->type (:meta field) known-types)
                  field-tv  (or declared (gen/fresh-tvar))
                  [init-tv init-node init-constr init-ctx] (if init-expr
                                                             (gen/cg-node-raw init-expr ctx)
                                                             [nil nil nil ctx])
                  eq-constr (when (and declared init-tv)
                              [(c/make-cequal declared init-tv)])
                  new-field (cond-> (assoc field :type field-tv)
                                    init-node (assoc :init init-node))]
              [(conj flds new-field)
               (into constrs (concat init-constr eq-constr))
               (or init-ctx ctx)]))
          [[] [] context]
          fields)
        record-tv   (gen/fresh-tvar)
        new-node    (-> node (assoc :fields new-fields) (ty/set-type! record-tv))
        ;; 记录类型加入已知类型，非环境
        new-ctx     (u/add-known-type final-ctx record-name)]
    [record-tv new-node (vec constrs) new-ctx]))
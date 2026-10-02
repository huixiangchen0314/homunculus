(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.record
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod gen/gen-node* :record [current-node env]
  (let [fields (n/record-fields current-node)
        ;; 顺序处理字段，累积环境
        [new-fields constraints final-env]
        (reduce
          (fn [[fields constraints current-env] field]
            (let [init-expr (:init field)
                  ;; 用统一入口获取字段声明类型
                  declared   (ty/meta->type (:meta field))
                  field-tv   (or declared (gen/fresh-tvar))
                  {init-type        :type
                   init-node        :node
                   init-constraints :constraints
                   init-env         :env}
                  (if init-expr
                    (gen/gen-node* init-expr current-env)
                    {:type        nil
                     :node        nil
                     :constraints nil
                     :env         current-env})
                  eq-constr  (when (and declared init-type)
                               [(cons/make-cequal declared init-type)])
                  new-field  (cond-> (assoc field :type field-tv)
                                     init-node (assoc :init init-node))]
              [(conj fields new-field)
               (into constraints (concat init-constraints eq-constr))
               init-env]))
          [[] [] env]
          fields)
        record-tv (gen/fresh-tvar)
        new-node  (-> current-node
                      (assoc :fields new-fields)
                      (ty/set-type! record-tv))]
    {:type        record-tv
     :node        new-node
     :constraints (vec constraints)
     :env         final-env}))
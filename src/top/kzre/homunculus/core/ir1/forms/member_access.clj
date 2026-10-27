(ns top.kzre.homunculus.core.ir1.forms.member-access
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; 关键字访问 (:foo obj) → obj.foo
(defmethod ir1/form->node :keyword-access [form]
  (let [[k obj] form]
    (n/make-member-access (ir1/->ir1 obj) k nil (meta form))))

;; 方法调用 (. obj method args...)
(defmethod ir1/form->node '. [form]
  (let [[_ obj method & args] form]
    (n/make-member-access (ir1/->ir1 obj)
                          method
                          (mapv ir1/->ir1 args)
                          (meta form))))
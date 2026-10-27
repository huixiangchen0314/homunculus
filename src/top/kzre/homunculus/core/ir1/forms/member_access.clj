(ns top.kzre.homunculus.core.ir1.forms.member-access
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; 关键字访问 (:foo obj) → obj.foo
(defmethod ir1/lower-node* :keyword-access [form env]
  (let [[k obj] form
        [obj-node env'] (ir1/lower-node obj env)]
    [(n/make-member-access obj-node k nil (meta form)) env']))

;; 方法调用 (. obj method args...)
(defmethod ir1/lower-node* '. [form env]
  (let [[_ obj method & args] form
        [obj-node env']   (ir1/lower-node obj env)
        [arg-nodes env''] (ir1/lower-seq args env')]
    [(n/make-member-access obj-node method arg-nodes (meta form)) env'']))
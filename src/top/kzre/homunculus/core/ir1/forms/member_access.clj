(ns top.kzre.homunculus.core.ir1.forms.member-access
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* :keyword-access [form env]
  (let [[k obj] form
        [obj-node env'] (ir1/lower-node obj env)]
    [(n/make-member-access obj-node k nil (meta form)) env']))

(defmethod ir1/lower-node* '. [form env]
  (let [[_ obj method & args] form
        [obj-node env']   (ir1/lower-node obj env)
        [arg-nodes env''] (ir1/lower-seq args env')]
    [(n/make-member-access obj-node method arg-nodes (meta form)) env'']))
(ns top.kzre.homunculus.core.ir1.forms.vector
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* :vector [form env]
  (let [[items env'] (ir1/lower-seq form env)]
    [(n/make-vector items (meta form)) env']))
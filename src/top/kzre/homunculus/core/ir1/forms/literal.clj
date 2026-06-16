(ns top.kzre.homunculus.core.ir1.forms.literal
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node :literal [form]
  (n/make-literal form))

(defmethod ir1/build-tree :literal [node]
  node)
(ns top.kzre.homunculus.core.ir1.forms.symbol
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node :symbol [form]
  (n/make-symbol form (meta form)))

(defmethod ir1/build-tree :symbol [node]
  node)
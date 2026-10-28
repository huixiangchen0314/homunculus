(ns top.kzre.homunculus.core.ir1.forms.symbol
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* :symbol [form env]
  (let [qualified (ir1/qualify-symbol env form)]
    [(n/make-symbol qualified (meta form)) env]))
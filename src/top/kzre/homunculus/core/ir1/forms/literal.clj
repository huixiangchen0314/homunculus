(ns top.kzre.homunculus.core.ir1.forms.literal
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* :literal [form env]
  [(n/make-literal form) env])
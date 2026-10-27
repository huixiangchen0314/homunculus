(ns top.kzre.homunculus.core.ir1.forms.ns
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.ast :as m]
            [top.kzre.homunculus.core.ir1.ns-form :as ns-form]))

(defmethod ir1/lower-node* 'ns [form env]
  (let [nsf  (ns-form/parse-form form)
        env' (ir1/bind-namespace env nsf)]
    [(m/->Ns (ns-form/ns-sym nsf)
             (vec (or (ns-form/reference-entry nsf :require) []))
             (ns-form/docstring nsf)
             (meta form))
     env']))
(ns top.kzre.homunculus.core.ir2.forms.variable
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :var [node env]
  (let [expr (n1/var-sym node)]
    (ir2/lower-ast expr env)))
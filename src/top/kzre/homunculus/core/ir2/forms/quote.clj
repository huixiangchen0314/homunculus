(ns top.kzre.homunculus.core.ir2.forms.quote
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :quote [node env]
  (let [kid (first (ir1p/children node))]
    (ir2/lower-ast kid env)))
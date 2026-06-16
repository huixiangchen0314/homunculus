(ns top.kzre.homunculus.core.ir2.forms.symbol
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :symbol [node env]
  [(n2/make-variable (name (n1/sym-name node)) {} (n1/node-meta node) nil)])
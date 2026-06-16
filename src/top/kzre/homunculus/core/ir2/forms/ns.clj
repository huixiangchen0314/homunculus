(ns top.kzre.homunculus.core.ir2.forms.ns
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :ns [node env]
  [(n2/make-ns (n1/namespace-name node)
               (n1/namespace-docstring node)
               (n1/namespace-attr-map node)
               (n1/namespace-references node)
               {}
               (n1/node-meta node)
               nil)])
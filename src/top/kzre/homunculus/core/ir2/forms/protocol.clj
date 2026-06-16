(ns top.kzre.homunculus.core.ir2.forms.protocol
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :protocol [node env]
  [(n2/make-protocol (n1/protocol-name node)
                     (n1/protocol-funcs node)
                     {}
                     (n1/node-meta node)
                     nil)])
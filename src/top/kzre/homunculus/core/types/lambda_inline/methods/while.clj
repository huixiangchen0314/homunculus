(ns top.kzre.homunculus.core.types.lambda-inline.methods.while
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :while [node config]
  (n/make-while (inline/eliminate-inline (n/while-test node) config)
                (inline/eliminate-inline (n/while-body node) config)
                (n/attrs node) (n/node-meta node) (n/parent node)))
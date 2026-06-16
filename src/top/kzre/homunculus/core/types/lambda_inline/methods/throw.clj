(ns top.kzre.homunculus.core.types.lambda-inline.methods.throw
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :throw [node config]
  (n/make-throw (inline/eliminate-inline (n/throw-expr node) config)
                (n/attrs node) (n/node-meta node) (n/parent node)))
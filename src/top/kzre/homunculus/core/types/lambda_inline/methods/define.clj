(ns top.kzre.homunculus.core.types.lambda-inline.methods.define
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :define [node config]
  (n/make-define (n/define-name node)
                 (inline/eliminate-inline (n/define-val node) config)
                 (n/define-doc node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))
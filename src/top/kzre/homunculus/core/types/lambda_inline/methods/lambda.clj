(ns top.kzre.homunculus.core.types.lambda-inline.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :lambda [node config]
  (n/make-lambda (mapv #(inline/eliminate-inline % config) (n/lambda-params node))
                 (inline/eliminate-inline (n/lambda-body node) config)
                 (n/lambda-captures node)
                 (n/lambda-fn-name node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))
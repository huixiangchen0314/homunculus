(ns top.kzre.homunculus.core.types.lambda-elim.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :lambda [node roots config defs]
  (n/make-lambda (mapv #(elim/eliminate % roots config defs) (n/lambda-params node))
                 (elim/eliminate (n/lambda-body node) roots config defs)
                 (n/lambda-captures node)
                 (n/lambda-fn-name node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))
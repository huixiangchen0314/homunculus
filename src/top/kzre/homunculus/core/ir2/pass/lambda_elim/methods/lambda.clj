(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.core :as elim]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.env :as p]))

(defmethod elim/elim-node* :lambda [node env]
  (let [param-names (map n/var-name (n/lambda-params node))
        inner-env   (reduce p/bind-var env param-names)
        [new-body body-defs] (elim/elim-node* (n/lambda-body node) inner-env)]
    [(n/make-lambda (n/lambda-params node) new-body
                    (n/lambda-captures node)
                    (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node))
     body-defs]))
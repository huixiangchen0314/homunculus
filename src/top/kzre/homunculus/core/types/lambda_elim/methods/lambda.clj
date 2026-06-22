(ns top.kzre.homunculus.core.types.lambda-elim.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :lambda [node config env]
  (let [param-names (map n/var-name (n/lambda-params node))
        inner-env   (into env param-names)   ; 扩展环境，加入参数名
        [new-body body-defs] (elim/eliminate (n/lambda-body node) config inner-env)]
    [(n/make-lambda (n/lambda-params node) new-body
                    (n/lambda-captures node)
                    (n/lambda-fn-name node)
                    (n/attrs node) (n/node-meta node) (n/parent node))
     body-defs]))
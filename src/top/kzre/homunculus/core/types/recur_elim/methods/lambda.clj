(ns top.kzre.homunculus.core.types.recur-elim.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :lambda [node]
  (n/make-lambda (mapv rec/eliminate (n/lambda-params node))
                 (rec/eliminate (n/lambda-body node))
                 (n/lambda-captures node)   ; 保留原始 captures（通常为空）
                 (n/lambda-fn-name node)    ; 保留原始函数名（符号或 nil）
                 (n/attrs node) (n/node-meta node) (n/parent node)))
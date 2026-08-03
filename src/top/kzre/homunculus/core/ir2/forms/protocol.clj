(ns top.kzre.homunculus.core.ir2.forms.protocol
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :protocol [node env]
  (let [ir1-methods (n1/protocol-methods node)   ;; IR1 Method 向量
        ir2-methods (mapv (fn [m]
                            ;; 降低每个 Method
                            (let [params (n1/method-params m)   ;; IR1 Param 向量
                                  ir2-params (mapv #(first (ir2/lower-ast % env)) params)]
                              (n2/make-method (n1/method-name m)
                                              ir2-params
                                              (n1/method-doc m)
                                              {}          ; attrs
                                              (n1/node-meta m))))
                          ir1-methods)]
    [(n2/make-protocol (n1/protocol-name node)
                       ir2-methods
                       {}
                       (n1/node-meta node))]))
(ns top.kzre.homunculus.core.types.check.methods.while
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :while [node expected context]
  (let [nil-type (ty/make-tcon :nil)]
    (when (and expected (not= expected nil-type))
      (throw (ex-info "while expression must have nil type"
                      {:expected expected})))
    (let [test-node (check/check-node (n/while-test node) (ty/make-tcon :bool) context)
          body-node (check/check-node (n/while-body node) nil context)]
      (n/make-while test-node body-node
                    (n/attrs node) (n/node-meta node) (n/parent node)))))
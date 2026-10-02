(ns top.kzre.homunculus.core.ir2.pass.check.methods.while
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :while [node expected env]
  (let [nil-type (ty/make-tcon :nil)]
    (when (and expected (not= expected nil-type))
      (throw (ex-info "while expression must have nil type"
                      {:expected expected})))
    (let [required-type (p/truthy-type env)
          test-expected (when required-type (ty/make-tcon required-type))
          test-node     (check/check-node* (n/while-test node) test-expected env)
          body-node     (check/check-node* (n/while-body node) nil env)]
      (n/make-while test-node body-node
                    (n/attrs node)
                    (n/node-meta node)))))
(ns top.kzre.homunculus.core.ir2.pass.check.methods.if
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :if [node expected env]
  (let [required-type (p/truthy-type env)
        test-expected (when required-type (ty/make-tcon required-type))]
    (n/make-if (check/check-node* (n/if-test node) test-expected env)
               (check/check-node* (n/if-then node) expected env)
               (when-let [else (n/if-else node)]
                 (check/check-node* else expected env))
               (n/attrs node)
               (n/node-meta node))))
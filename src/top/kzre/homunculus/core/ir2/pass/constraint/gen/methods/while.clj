(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.while
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod gen/gen-node* :while [current-node env]
  (let [{test-type        :type
         test-node        :node
         test-constraints :constraints
         test-env         :env}
        (gen/gen-node* (n/while-test current-node) env)
        {body-type        :type
         body-node        :node
         body-constraints :constraints
         body-env         :env}
        (gen/gen-node* (n/while-body current-node) test-env)
        test-eq  (when-let [required-type (p/truthy-type env)]
                   (when test-type
                     [(cons/make-cequal test-type (ty/make-tcon required-type))]))
        new-node (n/make-while test-node body-node
                               (n/attrs current-node)
                               (n/node-meta current-node))]
    {:type        body-type
     :node        (ty/set-type! new-node nil)
     :constraints (concat test-constraints body-constraints test-eq)
     :env         body-env}))
(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.binding
  (:require [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
            [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
            [clojure.string :as str]
            [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]))


(defmethod gen/gen-node* :binding [node env]
  (let [ var-n (:name (:var node))
        {var-type :type var-node :node var-cons :constraints var-env :env}
        (gen/gen-node-hinted (:var node) env)
        var-name (:name var-node)
        {val-type :type val-node :node val-cons :constraints  val-env :env}
        (gen/gen-node-hinted (:val node) var-env)
        binding-type (p/generalize env val-type)
        cons-eq   (cons/make-cequal var-type binding-type)
        new-node  (assoc node :var var-node :val val-node)]
    (gen/result var-type new-node (p/bind-var val-env var-name var-type)
                var-cons val-cons cons-eq)))
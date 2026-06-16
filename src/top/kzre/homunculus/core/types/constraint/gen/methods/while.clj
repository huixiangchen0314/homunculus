(ns top.kzre.homunculus.core.types.constraint.gen.methods.while
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :while [node context]
  (let [[test-tv test-node test-constr] (gen/cg-node-raw (n/while-test node) context)
        [body-tv body-node body-constr] (gen/cg-node-raw (n/while-body node) context)
        tv (ty/make-tcon :nil)
        test-eq (list (c/make-cequal test-tv (ty/make-tcon :bool)))
        new-node (n/make-while test-node body-node
                               (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv) (concat test-constr body-constr test-eq)]))
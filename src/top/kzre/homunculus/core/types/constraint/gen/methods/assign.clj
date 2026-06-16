(ns top.kzre.homunculus.core.types.constraint.gen.methods.assign
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :assign [node context]
  (let [[var-tv var-node var-constr] (gen/cg-node-raw (n/assign-var node) context)
        [val-tv val-node val-constr] (gen/cg-node-raw (n/assign-val node) context)
        tv (ty/make-tcon :nil)
        new-node (n/make-assign var-node val-node
                                (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv)
     (concat var-constr val-constr (list (c/make-cequal var-tv val-tv)))]))
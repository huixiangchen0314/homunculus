(ns top.kzre.homunculus.core.types.constraint.gen.methods.member-access
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :member-access [node context]
  (let [[target-tv target-node target-constr] (gen/cg-node-raw (n/access-target node) context)
        args (n/access-args node)
        results (mapv #(gen/cg-node-raw % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constr (mapcat #(nth % 2) results)
        tv (gen/fresh-tvar)
        new-node (n/make-member-access target-node
                                       (n/access-member node)
                                       arg-nodes
                                       (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv) (concat target-constr arg-constr)]))
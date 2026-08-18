(ns top.kzre.homunculus.core.types.constraint.gen.methods.member-access
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.types.constraint.constraints.core :as cons]
   [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
   [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :member-access [node context]
  (let [[target-tv target-node target-constr target-ctx] (gen/cg-node-raw (n/access-target node) context)
        args (n/access-args node)
        [arg-results final-ctx]
        (reduce (fn [[results ctx] arg]
                  (let [[arg-tv arg-node arg-constr arg-ctx] (gen/cg-node-raw arg ctx)]
                    [(conj results {:tv arg-tv :node arg-node :constr arg-constr})
                     arg-ctx]))
                [[] target-ctx]
                args)
        arg-tys (mapv :tv arg-results)
        arg-nodes (mapv :node arg-results)
        arg-constr (mapcat :constr arg-results)
        ret-tv (gen/fresh-tvar)
        proj-constr (cons/make-cproject target-tv (n/access-member node) ret-tv)
        new-node (n/make-member-access target-node
                                       (n/access-member node)
                                       arg-nodes
                                       (n/attrs node)          ; 补充 attrs
                                       (n/node-meta node)
                                       )]
    [ret-tv (ty/set-type! new-node ret-tv)
     (concat target-constr arg-constr [proj-constr])
     final-ctx]))
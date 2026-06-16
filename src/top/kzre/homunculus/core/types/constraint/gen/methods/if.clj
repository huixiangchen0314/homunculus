(ns top.kzre.homunculus.core.types.constraint.gen.methods.if
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :if [node context]
  (let [[test-tv test-node test-constr] (gen/cg-node-raw (n/if-test node) context)
        [then-tv then-node then-constr] (gen/cg-node-raw (n/if-then node) context)
        [else-tv else-node else-constr] (if-let [else (n/if-else node)]
                                          (gen/cg-node-raw else context)
                                          [nil nil nil])
        tv (gen/fresh-tvar)
        test-eq (when test-tv (list (c/make-cequal test-tv (ty/make-tcon :bool))))
        branch-eq (if else-tv
                    [(c/make-cequal then-tv tv) (c/make-cequal else-tv tv)]
                    [(c/make-cequal then-tv tv)])
        new-node (n/make-if test-node then-node else-node
                            (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv)
     (concat test-constr then-constr else-constr test-eq branch-eq)]))
(ns top.kzre.homunculus.core.types.constraint.gen.methods.if
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :if [node context]
  (let [[test-tv test-node test-constr] (gen/cg-node-raw (:test node) context)
        [then-tv then-node then-constr] (gen/cg-node-raw (:then node) context)
        [else-tv else-node else-constr] (if (:else node)
                                          (gen/cg-node-raw (:else node) context)
                                          [nil nil nil])
        tv (gen/fresh-tvar)
        test-eq (when test-tv (list (cm/->CEqual test-tv (t/->TCon :bool))))
        branch-eq (if else-tv
                    [(cm/->CEqual then-tv tv) (cm/->CEqual else-tv tv)]
                    [(cm/->CEqual then-tv tv)])
        new-node (m/->IfNode test-node then-node else-node (:attrs node) (:meta node) (:parent node))]
    [tv (ty/set-type! new-node tv)
     (concat test-constr then-constr else-constr test-eq branch-eq)]))
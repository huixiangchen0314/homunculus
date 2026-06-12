(ns top.kzre.homunculus.core.types.elaborate.methods.let
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]
            [top.kzre.homunculus.core.types.subst :as subst]
            [top.kzre.homunculus.core.types.alpha-rename :as alpha]))

(defmethod eliminate :let [node ir2-roots config new-defs]
  (let [bindings (:bindings node)
        [processed-bindings new-body-pre]
        (reduce (fn [[bnds body] [var val]]
                  (let [processed-val (eliminate val ir2-roots config new-defs)]
                    (if (= (ir2p/kind processed-val) :lambda)
                      (let [renamed-lam (alpha/rename processed-val)
                            new-body' (subst/inline-expr body (:name var) renamed-lam)]
                        [bnds new-body'])
                      [(conj bnds [(eliminate var ir2-roots config new-defs) processed-val]) body])))
                [[] (:body node)]
                bindings)]
    (let [processed-body (eliminate new-body-pre ir2-roots config new-defs)]
      (if (seq processed-bindings)
        (m/->LetNode (vec processed-bindings) processed-body (:attrs node) (:meta node) (:parent node))
        processed-body))))
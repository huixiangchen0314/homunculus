(ns top.kzre.homunculus.core.types.constraint.gen.methods.call
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :call [node context]
  (let [[fn-tv fn-node fn-constraints] (gen/cg-node-raw (:fn node) context)
        builtin-candidates (get-in fn-node [:attrs :builtin-fn])
        candidates (if (seq builtin-candidates) builtin-candidates fn-tv)
        args (:args node)
        results (map #(gen/cg-node-raw % context) args)
        arg-tys (mapv first results)
        arg-nodes (mapv second results)
        arg-constraints (mapcat #(nth % 2) results)
        ret-tv (gen/fresh-tvar)]
    (if (and (not (satisfies? tp/IType candidates))
             (coll? candidates)
             (seq candidates)
             (not (scheme/tscheme? (first candidates))))
      [ret-tv
       (ty/set-type! node ret-tv)
       (concat (list (cm/->COverload candidates arg-tys ret-tv node))
               fn-constraints arg-constraints)]
      (let [desired (reduce (fn [ret arg] (t/->TFun arg ret)) ret-tv (reverse arg-tys))
            new-node (m/->CallNode fn-node (vec arg-nodes) (:attrs node) (:meta node) (:parent node))]
        [ret-tv
         (ty/set-type! new-node ret-tv)
         (concat (list (cm/->CEqual fn-tv desired))
                 fn-constraints arg-constraints)]))))
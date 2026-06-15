(ns top.kzre.homunculus.core.types.constraint.gen.methods.let
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :let [node context]
  (let [bindings (:bindings node)
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr] (gen/cg-node-raw val (assoc context :env env))
                  var-name (:name var)
                  binding (if (ty/fun-type? val-tv)
                            (scheme/generalize val-tv env)
                            val-tv)
                  var-node (ty/set-type! var binding)]
              [(conj bnds [var-node val-node])
               (e/extend-env env var-name binding)
               (concat constrs val-constr)]))
          [[] (:env context) []]
          bindings)
        [body-tv body-node body-constraints] (gen/cg-node-raw (:body node) (assoc context :env new-env))]
    [body-tv
     (ty/set-type! (m/->LetNode (vec bind-nodes) body-node (:attrs node) (:meta node) (:parent node))
                   body-tv)
     (concat bind-constraints body-constraints)]))
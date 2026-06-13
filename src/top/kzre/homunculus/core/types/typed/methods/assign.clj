(ns top.kzre.homunculus.core.types.typed.methods.assign
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :assign [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [[var-ty var-node s-var] (infer/infer (:var node) context)
          [val-ty val-node s-val] (infer/infer (:val node) context)
          s1 (merge s-var s-val)
          s-unify (u/unify (u/substitute var-ty s1) (u/substitute val-ty s1))
          s-final (merge s1 s-unify)
          ty (t/->TCon :nil)
          new-node (type/set-type! (assoc node :var var-node :val val-node) ty)]
      [ty new-node s-final])))
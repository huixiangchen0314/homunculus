(ns top.kzre.homunculus.core.types.typed.methods.while
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :while [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [[test-ty test-node s-test] (infer/infer (:test node) context)
          [body-ty body-node s-body] (infer/infer (:body node) context)
          s1 (merge s-test s-body)
          s-unify (u/unify (u/substitute test-ty s1) (u/substitute (t/->TCon :bool) s1))
          s-final (merge s1 s-unify)
          body-ty' (u/substitute body-ty s-final)
          new-node (type/set-type! (assoc node :test test-node :body body-node) body-ty')]
      [body-ty' new-node s-final])))
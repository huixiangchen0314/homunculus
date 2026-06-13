(ns top.kzre.homunculus.core.types.typed.methods.let
  (:require [clojure.set :as set]
            [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.typed.scheme :as s]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import (top.kzre.homunculus.core.types.typed.scheme TScheme)))

(defmethod infer/infer :let [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [bindings (:bindings node)
          [bind-nodes env-ext s-bindings]
          (reduce (fn [[bnds env subst] [var-node val-node]]
                    (let [[val-ty val-node' s-val] (infer/infer val-node (assoc context :env env))
                          subst' (merge subst s-val)
                          val-ty' (u/substitute val-ty subst')
                          annot-ty (when-let [f (:frontend context)] (tp/meta->type f var-node))
                          env-subst (infer/apply-subst-to-env env subst')
                          binding (if annot-ty
                                    (let [annot-ty' (u/substitute annot-ty subst')]
                                      (u/unify val-ty' annot-ty')
                                      annot-ty')
                                    (s/generalize val-ty' env-subst))
                          var-name (:name var-node)
                          env2 (e/extend-env env var-name binding)
                          var-node' (if (instance? TScheme binding)
                                      var-node
                                      (type/set-type! var-node binding))]
                      [(conj bnds [var-node' val-node']) env2 subst']))
                  [[] (:env context) {}]
                  bindings)
          env-body (infer/apply-subst-to-env env-ext s-bindings)
          [body-ty body-node s-body] (infer/infer (:body node) (assoc context :env env-body))
          s-final (merge s-bindings s-body)
          overall-ty (u/substitute body-ty s-final)
          new-node (type/set-type! (assoc node :bindings (vec bind-nodes) :body body-node) overall-ty)]
      [overall-ty new-node s-final])))
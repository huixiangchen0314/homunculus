(ns top.kzre.homunculus.core.types.typed.methods.loop
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :loop [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [bindings (:bindings node)
          [bind-nodes env-ext s-bindings]
          (reduce (fn [[bnds env subst] [var-node val-node]]
                    (let [[val-ty val-node' s-val] (infer/infer val-node (assoc context :env env))
                          var-name (:name var-node)
                          env2 (e/extend-env env var-name val-ty)
                          var-node' (type/set-type! var-node val-ty)]
                      [(conj bnds [var-node' val-node']) env2 (merge subst s-val)]))
                  [[] (:env context) {}]
                  bindings)
          loop-var-names (mapv (fn [[v _]] (:name v)) bind-nodes)
          env-loop (assoc env-ext :ir2/loop-vars loop-var-names)
          [body-ty body-node s-body] (infer/infer (:body node) (assoc context :env env-loop))
          s (merge s-bindings s-body)
          new-node (type/set-type! (assoc node :bindings (vec bind-nodes) :body body-node) body-ty)]
      [body-ty new-node s])))

(defmethod infer/infer :recur [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [loop-var-names (get (:env context) :ir2/loop-vars)
          _ (when-not loop-var-names (throw (ex-info "recur outside loop" {})))
          args (:args node)
          _ (when (not= (count args) (count loop-var-names))
              (throw (ex-info "recur arg count mismatch" {})))
          [arg-tys arg-nodes s-args]
          (loop [arg-irs args, tys [], nodes [], subst {}]
            (if (seq arg-irs)
              (let [[arg-ty arg-node s-arg] (infer/infer (first arg-irs) context)]
                (recur (rest arg-irs) (conj tys arg-ty) (conj nodes arg-node) (merge subst s-arg)))
              [tys nodes subst]))
          _ (doseq [[arg-ty var-name] (map vector arg-tys loop-var-names)]
              (let [var-ty (e/lookup-env (:env context) var-name)]
                (u/unify arg-ty var-ty)))
          ty (t/->TCon :nil)
          new-node (type/set-type! (assoc node :args (vec arg-nodes)) ty)]
      [ty new-node s-args])))
(ns top.kzre.homunculus.core.types.typed.methods.loop
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.types.typed.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :loop [node context]
  (let [bindings (:bindings node)
        [bind-nodes new-env]
        (reduce (fn [[bnds env] [var-node val-node]]
                  (let [[val-ty val-new] (infer/infer val-node (assoc context :env env))
                        var-name (:name var-node)
                        env2 (e/extend-env env var-name val-ty)
                        var-new (assoc-in var-node [:attrs :type] val-ty)]
                    [(conj bnds [var-new val-new]) env2]))
                [[] (:env context)]
                bindings)
        loop-var-names (mapv (fn [[v _]] (:name v)) bind-nodes)
        env-loop (assoc new-env :ir2/loop-vars loop-var-names)
        [body-ty body-node] (infer/infer (:body node) (assoc context :env env-loop))
        new-attrs (assoc (ir2p/attrs node) :type body-ty)
        new-node (assoc node :bindings (vec bind-nodes) :body body-node :attrs new-attrs)]
    [body-ty new-node]))

(defmethod infer/infer :recur [node context]
  (let [loop-var-names (get (:env context) :ir2/loop-vars)
        _ (when-not loop-var-names
            (throw (ex-info "recur outside loop" {})))
        args (:args node)
        _ (when (not= (count args) (count loop-var-names))
            (throw (ex-info "recur arg count mismatch" {})))]
    (doseq [[arg var-name] (map vector args loop-var-names)]
      (let [[arg-ty _] (infer/infer arg context)
            var-ty (e/lookup-env (:env context) var-name)]
        (u/unify arg-ty var-ty)))
    (let [ty (t/->TCon :nil)
          new-attrs (assoc (ir2p/attrs node) :type ty)
          new-node (assoc node :attrs new-attrs)]
      [ty new-node])))
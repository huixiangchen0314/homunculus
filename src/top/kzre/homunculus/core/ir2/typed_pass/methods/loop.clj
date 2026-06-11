(ns top.kzre.homunculus.core.ir2.typed-pass.methods.loop
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.env :as e]
            [top.kzre.homunculus.core.ir2.typed-pass.unify :as u]))

(defmethod infer/infer :loop [node env]
  (let [bindings (:bindings node)
        [bind-nodes env']
        (reduce (fn [[bnds env] [var-node val-node]]
                  (let [[val-ty val-new] (infer/infer val-node env)
                        var-name (:name var-node)
                        env2 (e/extend-env env var-name val-ty)
                        var-new (assoc-in var-node [:attrs :type] val-ty)]
                    [(conj bnds [var-new val-new]) env2]))
                [[] env] bindings)
        env-loop (assoc env' :ir2/loop-vars (mapv (fn [[v _]] (:name v)) bind-nodes))
        [body-ty body-node] (infer/infer (:body node) env-loop)]
    [body-ty (assoc node :bindings (vec bind-nodes) :body body-node
                         :attrs (assoc (:attrs node) :type body-ty))]))

(defmethod infer/infer :recur [node env]
  (let [loop-var-names (get env :ir2/loop-vars)
        _ (when-not loop-var-names
            (throw (ex-info "recur outside loop" {})))
        args (:args node)
        _ (when (not= (count args) (count loop-var-names))
            (throw (ex-info "recur arg count mismatch" {})))]
    (doseq [[arg var-name] (map vector args loop-var-names)]
      (let [[arg-ty _] (infer/infer arg env)
            var-ty (e/lookup-env env var-name)]
        (u/unify arg-ty var-ty)))
    [(t/->TCon :nil) (assoc-in node [:attrs :type] (t/->TCon :nil))]))
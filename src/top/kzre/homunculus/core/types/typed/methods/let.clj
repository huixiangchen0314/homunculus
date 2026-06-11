(ns top.kzre.homunculus.core.types.typed.methods.let
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :let [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node]
    (let [bindings (:bindings node)
          [bind-nodes new-env]
          (reduce (fn [[bnds env] [var-node val-node]]
                    (let [[val-ty val-new] (infer/infer val-node (assoc context :env env))
                          annot-ty (when-let [f (:frontend context)] (tp/meta->type f var-node))
                          var-ty (or annot-ty val-ty)
                          var-name (:name var-node)
                          env2 (e/extend-env env var-name var-ty)
                          var-new (assoc-in var-node [:attrs :type] var-ty)]
                      [(conj bnds [var-new val-new]) env2]))
                  [[] (:env context)]
                  bindings)
          [body-ty body-node] (infer/infer (:body node) (assoc context :env new-env))
          new-attrs (assoc (ir2p/attrs node) :type body-ty)
          new-node (assoc node :bindings (vec bind-nodes) :body body-node :attrs new-attrs)]
      [body-ty new-node])))
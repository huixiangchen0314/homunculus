(ns top.kzre.homunculus.core.types.typed.methods.variable
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.typed.scheme :as s]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import (top.kzre.homunculus.core.types.typed.scheme TScheme)))

(defmethod infer/infer :variable [node context]
  ;; 如果节点已有类型（来自 infer-pass 或标注），直接返回
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [frontend (:frontend context)
          env (:env context)
          var-name (:name node)
          binding (or (when frontend (tp/meta->type frontend node))
                      (e/lookup-env env var-name)
                      (e/lookup-env env (symbol var-name))
                      (throw (ex-info "Unbound variable" {:name var-name})))
          ty (if (instance? TScheme binding)
               (s/instantiate binding)
               binding)]
      [ty (assoc-in node [:attrs :type] ty) {}])))
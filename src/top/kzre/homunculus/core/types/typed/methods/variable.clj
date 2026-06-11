;; types/typed/methods/variable.clj
(ns top.kzre.homunculus.core.types.typed.methods.variable
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :variable [node context]
  ;; 如果 infer‑pass 已经推导出类型，直接使用
  (if-let [existing (get-in node [:attrs :type])]
    [existing node]
    (let [frontend (:frontend context)
          env (:env context)
          var-name (:name node)
          ty (or (when frontend (tp/meta->type frontend node))
                 (e/lookup-env env var-name)
                 (e/lookup-env env (symbol var-name))
                 (throw (ex-info "Unbound variable" {:name var-name})))]
      [ty (assoc-in node [:attrs :type] ty)])))
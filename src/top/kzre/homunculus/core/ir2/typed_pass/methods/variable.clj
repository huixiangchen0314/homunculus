(ns top.kzre.homunculus.core.ir2.typed-pass.methods.variable
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.env :as e]))

(defmethod infer/infer :variable [node env]
  (let [ty (or (t/meta-type node)
               (e/lookup-env env (:name node))
               (throw (ex-info "Unbound variable" {:name (:name node)})))]
    [ty (assoc-in node [:attrs :type] ty)]))
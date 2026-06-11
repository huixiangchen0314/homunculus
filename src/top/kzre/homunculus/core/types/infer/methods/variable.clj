;; top.kzre.homunculus.core.types.infer.methods.variable.clj
(ns top.kzre.homunculus.core.types.infer.methods.variable
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/local-infer :variable [node context]
  (let [frontend (:frontend context)
        env (:env context)
        var-name (:name node)
        ty (or (when frontend (tp/meta->type frontend node))
               (e/lookup-env env var-name)
               (e/lookup-env env (symbol var-name)))]
    (if ty
      (infer/success ty (assoc-in node [:attrs :type] ty))
      (infer/nothing node))))
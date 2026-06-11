(ns top.kzre.homunculus.core.types.typed.methods.variable
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.typed.env :as e]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :variable [node context]
  (let [frontend (:frontend context)
        env (:env context)
        var-name (:name node)   ;; 字符串
        ;; 同时尝试字符串和符号查找
        ty (or (when frontend (tp/meta->type frontend node))
               (e/lookup-env env var-name)
               (e/lookup-env env (symbol var-name))
               (throw (ex-info "Unbound variable" {:name var-name})))
        new-attrs (assoc (ir2p/attrs node) :type ty)
        new-node (assoc node :attrs new-attrs)]
    [ty new-node]))
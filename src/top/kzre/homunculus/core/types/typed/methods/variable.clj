(ns top.kzre.homunculus.core.types.typed.methods.variable
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.typed.scheme :as s]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import (top.kzre.homunculus.core.types.typed.scheme TScheme)))

(defmethod infer/infer :variable [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [frontend (:frontend context)
          env (:env context)
          var-name (:name node)
          binding (or (when frontend (tp/meta->type frontend node))
                      (e/lookup-env env var-name)
                      (e/lookup-env env (symbol var-name)))
          ty (if binding
               (if (instance? TScheme binding)
                 (s/instantiate binding)
                 binding)
               (t/->TVar (gensym "var")))]   ;; 未绑定 -> 自由类型变量
      [ty (type/set-type! node ty) {}])))
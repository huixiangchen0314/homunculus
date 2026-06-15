;; top.kzre.homunculus.core.types.infer.methods.variable
(ns top.kzre.homunculus.core.types.infer.methods.variable
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.types.env :as e]
   [top.kzre.homunculus.core.types.infer.core :as infer]
   [top.kzre.homunculus.core.types.protocol :as tp]
   [top.kzre.homunculus.core.types.type :as t]))

(defmethod infer/local-infer :variable [node context]
  (let [env (infer/env context)
        name (n/var-name node)
        ;; 1. 节点已有类型（例如通过 ty/set-type! 预先设置）
        existing (t/get-type node)]
    (if existing
      (infer/success existing node)
      (let [binding (e/lookup-env env name)]
        (if binding
          (infer/success binding (t/ensure-type node binding))
          ;; 2. 检查前端内置函数
          (if-let [frontend (infer/frontend context)]
            (if-let [builtin-ty (get (tp/builtin-functions frontend) name)]
              (infer/success builtin-ty (t/ensure-type node builtin-ty))
              (infer/nothing node))
            (infer/nothing node)))))))
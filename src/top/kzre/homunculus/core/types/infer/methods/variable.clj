;; top.kzre.homunculus.core.types.infer.methods.variable.clj
(ns top.kzre.homunculus.core.types.infer.methods.variable
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.infer.core :as c]
            [top.kzre.homunculus.core.types.type :as t]))

;; 提取节点标注类型，转换为内部类型表示
(defmethod c/local-infer :variable [node context]
  (let [frontend (c/frontend context)
        env (c/env context)
        var-name (n/var-name node)
        type (or (when frontend (t/frontend-type node frontend))
               (e/lookup-env env var-name)
               (e/lookup-env env (symbol var-name)))]
    (if type
      (c/success type (t/ensure-type node type))
      (c/nothing node))))
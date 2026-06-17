(ns top.kzre.homunculus.core.types.infer.methods.variable
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.infer.core :as infer]
    [top.kzre.homunculus.core.types.type :as t]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defmethod infer/local-infer :variable [node context]
  (let [env (infer/env context)
        name (n/var-name node)
        existing (t/get-type node)]
    (if existing
      (infer/success existing node)
      (let [binding (e/lookup-env env name)]
        (if binding
          (infer/success binding (t/ensure-type node binding))
          ;; 使用符号表查询
          (if-let [entry (sym/lookup-in-tables name (:symbol-table context))]
            (cond
              (sym/function-symbol? entry)
              ;; 函数类型不在此处确定，交给约束求解处理
              (infer/nothing node)
              (sym/record-symbol? entry)
              (infer/success (:type entry) (t/ensure-type node (:type entry)))
              (sym/variable-symbol? entry)
              (infer/success (:type entry) (t/ensure-type node (:type entry)))
              :else
              (infer/nothing node))
            (infer/nothing node)))))))
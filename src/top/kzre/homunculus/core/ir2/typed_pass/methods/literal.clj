(ns top.kzre.homunculus.core.ir2.typed-pass.methods.literal
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]))

(defmethod infer/infer :literal [node env]
  (let [ty (or (t/meta-type node) (t/value->type (:val node)))]
    [ty (assoc-in node [:attrs :type] ty)]))
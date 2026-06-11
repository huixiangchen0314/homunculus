(ns top.kzre.homunculus.core.ir2.typed-pass.methods.default
  (:require [top.kzre.homunculus.core.ir2.typed-pass.core :as infer]))

(defmethod infer/infer :default [node env]
  (throw (ex-info "Type inference not implemented" {:kind (:kind node)})))
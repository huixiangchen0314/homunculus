(ns top.kzre.homunculus.core.ir2.pass.lambda-inline.api
  "Lambda 内联 Pass 的公共 API。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.lambda-inline.core :as core]))

(def inline-nodes core/inline-nodes)
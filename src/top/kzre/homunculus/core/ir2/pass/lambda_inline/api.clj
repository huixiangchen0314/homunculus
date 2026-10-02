(ns top.kzre.homunculus.core.ir2.pass.lambda-inline.api
  "Lambda 内联 Pass 的公共 API。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.lambda-inline.env]
    [top.kzre.homunculus.core.ir2.pass.lambda-inline.core]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.core.ir2.pass.lambda-inline.core
   :refer [inline]])

(re-export
  [top.kzre.homunculus.core.ir2.pass.lambda-inline.env
   :refer [make-env]])
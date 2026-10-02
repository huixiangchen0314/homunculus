(ns top.kzre.homunculus.core.ir2.pass.inline.api
  "内联相关 Pass 的统一入口。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.inline.core]
    [top.kzre.homunculus.core.ir2.pass.inline.env]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))

(re-export
  [top.kzre.homunculus.core.ir2.pass.inline.core
   :refer
   [inline]])

(re-export
  [top.kzre.homunculus.core.ir2.pass.inline.env
   :refer
   [make-env]])
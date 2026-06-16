(ns top.kzre.homunculus.core.types.subst.api
  "替换工具集合的公共 API，重新导出内联、提升和表达式替换。"
  (:require [top.kzre.homunculus.core.types.subst.inline :as inline]
            [top.kzre.homunculus.core.types.subst.lift :as lift]
            [top.kzre.homunculus.core.types.subst.replace :as replace]))

(def inline-call inline/inline-call)
(def lift-lambda lift/lift-lambda)
(def replace-var replace/replace-var)
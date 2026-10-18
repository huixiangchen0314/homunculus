(ns top.kzre.homunculus.backend.glsl.emit.api
  (:require
    [top.kzre.homunculus.backend.glsl.emit.env]
    [top.kzre.homunculus.backend.glsl.emit.core]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.backend.glsl.emit.env
   :refer [make-env]])

(re-export
  [top.kzre.homunculus.backend.glsl.emit.core
   :refer [emit]])
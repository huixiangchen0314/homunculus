(ns top.kzre.homunculus.backend.glsl.sematic.api
  (:require
    [top.kzre.homunculus.backend.glsl.sematic.config]
    [top.kzre.homunculus.backend.glsl.sematic.core]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.backend.glsl.sematic.core
   :refer [plan]])

(re-export
  [top.kzre.homunculus.backend.glsl.sematic.config
   :refer [make-config]])
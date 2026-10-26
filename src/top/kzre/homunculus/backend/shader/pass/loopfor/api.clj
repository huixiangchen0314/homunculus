(ns top.kzre.homunculus.backend.shader.pass.loopfor.api
  (:require
    [top.kzre.homunculus.backend.shader.pass.loopfor.core]
    [top.kzre.homunculus.backend.shader.pass.loopfor.env]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.backend.shader.pass.loopfor.core
   :refer [run]])

(re-export
  [top.kzre.homunculus.backend.shader.pass.loopfor.env
   :refer [make-env]])
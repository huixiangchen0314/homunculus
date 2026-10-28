(ns top.kzre.homunculus.frontend.shader.pass.loopfor.api
  (:require
    [top.kzre.homunculus.frontend.shader.pass.loopfor.core]
    [top.kzre.homunculus.frontend.shader.pass.loopfor.env]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.frontend.shader.pass.loopfor.core
   :refer [run]])

(re-export
  [top.kzre.homunculus.frontend.shader.pass.loopfor.env
   :refer [make-env]])
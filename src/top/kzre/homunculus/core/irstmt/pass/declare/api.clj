(ns top.kzre.homunculus.core.irstmt.pass.declare.api
  (:require
    [top.kzre.homunculus.core.irstmt.pass.declare.polyfill]
    [top.kzre.homunculus.core.irstmt.pass.declare.strip]
    [top.kzre.homunculus.core.irstmt.pass.declare.env]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.core.irstmt.pass.declare.polyfill
   :refer [polyfill polyfill-function]])


(re-export
  [top.kzre.homunculus.core.irstmt.pass.declare.strip
   :refer [strip strip-node]])

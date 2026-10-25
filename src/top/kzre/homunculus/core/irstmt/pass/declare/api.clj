(ns top.kzre.homunculus.core.irstmt.pass.declare.api
  "声明的消除重建，目的是不让CFG 考虑作用域"
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

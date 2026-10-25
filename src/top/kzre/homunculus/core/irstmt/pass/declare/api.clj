(ns top.kzre.homunculus.core.irstmt.pass.declare.api
  (:require
    [top.kzre.homunculus.core.irstmt.pass.declare.core]
    [top.kzre.homunculus.core.irstmt.pass.declare.env]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.core.irstmt.pass.declare.core
   :refer [decl]])


(re-export
  [top.kzre.homunculus.core.irstmt.pass.declare.env
   :refer [make-env]])

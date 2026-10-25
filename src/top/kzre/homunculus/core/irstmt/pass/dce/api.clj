(ns top.kzre.homunculus.core.irstmt.pass.dce.api
  (:require
    [top.kzre.homunculus.core.irstmt.pass.dce.core]
    [top.kzre.homunculus.core.irstmt.pass.dce.env]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.core.irstmt.pass.dce.core
   :refer [elim elim-node fx? ]])

(re-export
  [top.kzre.homunculus.core.irstmt.pass.dce.env
   :refer [make-env]])
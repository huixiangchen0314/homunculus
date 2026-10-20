(ns top.kzre.homunculus.core.irstmt.pass.assign-propagate.api
  (:require
    [top.kzre.homunculus.core.irstmt.pass.assign-propagate.env]
    [top.kzre.homunculus.core.irstmt.pass.assign-propagate.core]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))

(re-export
  [top.kzre.homunculus.core.irstmt.pass.assign-propagate.core
   :refer [propagate]])

(re-export
  [top.kzre.homunculus.core.irstmt.pass.assign-propagate.env
   :refer [make-env]])
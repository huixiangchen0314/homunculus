(ns top.kzre.homunculus.core.irstmt.transform.declare.api
  (:require
    [top.kzre.homunculus.core.irstmt.transform.declare.polyfill]
    [top.kzre.homunculus.core.irstmt.transform.declare.strip]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))



(re-export
  [top.kzre.homunculus.core.irstmt.transform.declare.polyfill
   :refer [polyfill-function]])


(re-export
  [top.kzre.homunculus.core.irstmt.transform.declare.strip
   :refer [strip-function]])

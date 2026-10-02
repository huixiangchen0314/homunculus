(ns top.kzre.homunculus.core.ir2.pass.ho-elim.api
  (:require
    [top.kzre.homunculus.core.ir2.pass.ho-elim.core]
    [top.kzre.homunculus.core.ir2.pass.ho-elim.env]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.core.ir2.pass.ho-elim.core
   :refer [elim]])

(re-export
  [top.kzre.homunculus.core.ir2.pass.ho-elim.env
   :refer [make-env]])
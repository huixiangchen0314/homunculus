(ns top.kzre.homunculus.core.ir2.pass.recur-elim.api
  (:require [top.kzre.homunculus.core.ir2.pass.recur-elim.core]
            [top.kzre.krro.core.util.re-export :refer [re-export]]))

(re-export
  [top.kzre.homunculus.core.ir2.pass.recur-elim.core
   :refer [elim]])


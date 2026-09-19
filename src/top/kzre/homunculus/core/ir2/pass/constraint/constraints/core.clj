(ns top.kzre.homunculus.core.ir2.pass.constraint.constraints.core
  (:require
   [top.kzre.homunculus.core.ir2.pass.constraint.constraints.equal :as equal]
   [top.kzre.homunculus.core.ir2.pass.constraint.constraints.overload :as overload]
   [top.kzre.homunculus.core.ir2.pass.constraint.constraints.project :as project]))

(def make-cequal equal/make-cequal)

(def make-cproject project/make-cproject)

(def make-coverload overload/make-coverload)
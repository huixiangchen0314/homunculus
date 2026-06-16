(ns top.kzre.homunculus.core.ir1.api
  (:require [top.kzre.homunculus.core.ir1.core :as core]
            [top.kzre.homunculus.core.ir1.forms.quote]
            [top.kzre.homunculus.core.ir1.forms.if]
            [top.kzre.homunculus.core.ir1.forms.do]
            [top.kzre.homunculus.core.ir1.forms.let]
            [top.kzre.homunculus.core.ir1.forms.fn]
            [top.kzre.homunculus.core.ir1.forms.def]
            [top.kzre.homunculus.core.ir1.forms.loop]
            [top.kzre.homunculus.core.ir1.forms.var]
            [top.kzre.homunculus.core.ir1.forms.set!]
            [top.kzre.homunculus.core.ir1.forms.vector]
            [top.kzre.homunculus.core.ir1.forms.call]
            [top.kzre.homunculus.core.ir1.forms.symbol]
            [top.kzre.homunculus.core.ir1.forms.literal]
            [top.kzre.homunculus.core.ir1.forms.map]
            [top.kzre.homunculus.core.ir1.forms.try]))

(def ->ir1 core/->ir1 )


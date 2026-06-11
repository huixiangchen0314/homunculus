(ns top.kzre.homunculus.core.ir1.forms
  "加载所有特殊形式的实现，使它们注册到 ir1.core 的 multimethod 中。"
  (:require [top.kzre.homunculus.core.ir1.forms.quote]
            [top.kzre.homunculus.core.ir1.forms.if]
            [top.kzre.homunculus.core.ir1.forms.do]
            [top.kzre.homunculus.core.ir1.forms.let]
            [top.kzre.homunculus.core.ir1.forms.fn]
            [top.kzre.homunculus.core.ir1.forms.def]
            [top.kzre.homunculus.core.ir1.forms.loop]
            [top.kzre.homunculus.core.ir1.forms.var-set]
            [top.kzre.homunculus.core.ir1.forms.throw]
            [top.kzre.homunculus.core.ir1.forms.try]))
(ns top.kzre.homunculus.core.ir2.forms
  (:require [top.kzre.homunculus.core.ir2.forms.define]
            [top.kzre.homunculus.core.ir2.forms.lambda]
            [top.kzre.homunculus.core.ir2.forms.if]
            [top.kzre.homunculus.core.ir2.forms.block]
            [top.kzre.homunculus.core.ir2.forms.let]
            [top.kzre.homunculus.core.ir2.forms.loop]      ;; ← 替换 while
            [top.kzre.homunculus.core.ir2.forms.try]
            [top.kzre.homunculus.core.ir2.forms.throw]
            [top.kzre.homunculus.core.ir2.forms.assign]
            [top.kzre.homunculus.core.ir2.forms.variable]))
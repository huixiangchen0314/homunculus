(ns top.kzre.homunculus.core.ir2.api
  (:require
    [top.kzre.homunculus.core.ir2.core :as ir2]
    ;; 加载所有 lower-ast 实现
    [top.kzre.homunculus.core.ir2.forms.literal]
    [top.kzre.homunculus.core.ir2.forms.symbol]
    [top.kzre.homunculus.core.ir2.forms.vector]
    [top.kzre.homunculus.core.ir2.forms.map]
    [top.kzre.homunculus.core.ir2.forms.call]
    [top.kzre.homunculus.core.ir2.forms.if]
    [top.kzre.homunculus.core.ir2.forms.do]
    [top.kzre.homunculus.core.ir2.forms.let]
    [top.kzre.homunculus.core.ir2.forms.fn]
    [top.kzre.homunculus.core.ir2.forms.def]
    [top.kzre.homunculus.core.ir2.forms.loop]
    [top.kzre.homunculus.core.ir2.forms.recur]
    [top.kzre.homunculus.core.ir2.forms.quote]
    [top.kzre.homunculus.core.ir2.forms.var]
    [top.kzre.homunculus.core.ir2.forms.set]
    [top.kzre.homunculus.core.ir2.forms.try]
    [top.kzre.homunculus.core.ir2.forms.ns]
    [top.kzre.homunculus.core.ir2.forms.record]
    [top.kzre.homunculus.core.ir2.forms.protocol]
    [top.kzre.homunculus.core.ir2.forms.member-access]))

;; re-export
(def lower ir2/lower)
(def ->ir2 ir2/->ir2)
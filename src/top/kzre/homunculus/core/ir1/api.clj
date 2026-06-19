(ns top.kzre.homunculus.core.ir1.api
  "IR1 的公共入口：加载所有特殊形式处理方法，并导出核心转换函数。"
  (:require [top.kzre.homunculus.core.ir1.core :as core]
    [top.kzre.homunculus.core.ir1.preprocess :as pre]
    ;; 基础字面量
            [top.kzre.homunculus.core.ir1.forms.literal]
            [top.kzre.homunculus.core.ir1.forms.symbol]
            [top.kzre.homunculus.core.ir1.forms.vector]
            [top.kzre.homunculus.core.ir1.forms.map]
    ;; 调用
            [top.kzre.homunculus.core.ir1.forms.call]
    ;; 控制流
            [top.kzre.homunculus.core.ir1.forms.if]
            [top.kzre.homunculus.core.ir1.forms.do]
    ;; 绑定
            [top.kzre.homunculus.core.ir1.forms.let]
            [top.kzre.homunculus.core.ir1.forms.fn]            ;; fn* 特殊形式
            [top.kzre.homunculus.core.ir1.forms.def]
            [top.kzre.homunculus.core.ir1.forms.loop]          ;; loop + recur
    ;; 引用 / 赋值
            [top.kzre.homunculus.core.ir1.forms.var]
            [top.kzre.homunculus.core.ir1.forms.set!]
            [top.kzre.homunculus.core.ir1.forms.quote]
    ;; 异常
            [top.kzre.homunculus.core.ir1.forms.try]           ;; try / catch / throw
    ;; 命名空间
            [top.kzre.homunculus.core.ir1.forms.ns]            ;; ns* 特殊形式
    ;; 记录 / 协议 / 成员访问
            [top.kzre.homunculus.core.ir1.forms.record]
            [top.kzre.homunculus.core.ir1.forms.protocol]
            [top.kzre.homunculus.core.ir1.forms.member-access]))

(def ->ir1 core/->ir1)

(def preprocess pre/preprocess)

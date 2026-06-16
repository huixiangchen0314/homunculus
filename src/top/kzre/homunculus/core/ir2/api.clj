(ns top.kzre.homunculus.core.ir2.api
  "IR2 lowering 的公共入口。加载所有 lower-ast 方法并重导出核心函数。"
  (:require
    [top.kzre.homunculus.core.ir2.core :as ir2]
    ;; 加载所有 lower-ast 实现（实际文件名与 IR1 种类对应）
    [top.kzre.homunculus.core.ir2.forms.literal]          ;; :literal
    [top.kzre.homunculus.core.ir2.forms.symbol]           ;; :symbol
    [top.kzre.homunculus.core.ir2.forms.vector]           ;; :vector
    [top.kzre.homunculus.core.ir2.forms.map]              ;; :map
    [top.kzre.homunculus.core.ir2.forms.call]             ;; :call
    [top.kzre.homunculus.core.ir2.forms.if]               ;; :if
    [top.kzre.homunculus.core.ir2.forms.block]            ;; :do → BlockNode
    [top.kzre.homunculus.core.ir2.forms.let]              ;; :let
    [top.kzre.homunculus.core.ir2.forms.lambda]           ;; :fn → LambdaNode
    [top.kzre.homunculus.core.ir2.forms.define]           ;; :def → DefineNode
    [top.kzre.homunculus.core.ir2.forms.loop]             ;; :loop
    [top.kzre.homunculus.core.ir2.forms.quote]            ;; :quote
    [top.kzre.homunculus.core.ir2.forms.variable]         ;; :var → VariableNode 展开
    [top.kzre.homunculus.core.ir2.forms.assign]           ;; :set! → AssignNode
    [top.kzre.homunculus.core.ir2.forms.try]              ;; :try, :catch, :throw
    [top.kzre.homunculus.core.ir2.forms.ns]               ;; :ns
    [top.kzre.homunculus.core.ir2.forms.record]           ;; :record
    [top.kzre.homunculus.core.ir2.forms.protocol]         ;; :protocol
    [top.kzre.homunculus.core.ir2.forms.member-access]))  ;; :member-access


(def ->ir2 ir2/->ir2)
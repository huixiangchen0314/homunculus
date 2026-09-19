(ns top.kzre.homunculus.core.ir2.pass.infer.api
  "局部类型推断的公共入口。加载所有 defmethod 并重导出核心函数。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.infer.core :as core]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.assign]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.block]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.call]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.convert]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.define]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.if]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.lambda]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.let]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.literal]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.loop]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.map]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.member-access]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.ns]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.protocol]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.record]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.recur]      ;; 包含 try / catch / throw
    [top.kzre.homunculus.core.ir2.pass.infer.methods.try]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.variable]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.vector]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.array]
    [top.kzre.homunculus.core.ir2.pass.infer.methods.while]))

(def make-context core/make-context)

;; re-export 核心入口
(def infer core/infer)
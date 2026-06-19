(ns top.kzre.homunculus.core.types.infer.api
  "局部类型推断的公共入口。加载所有 defmethod 并重导出核心函数。"
  (:require
    [top.kzre.homunculus.core.types.infer.core :as c]
    [top.kzre.homunculus.core.types.infer.core :as core]
    [top.kzre.homunculus.core.types.infer.methods.assign]
    [top.kzre.homunculus.core.types.infer.methods.block]
    [top.kzre.homunculus.core.types.infer.methods.call]
    [top.kzre.homunculus.core.types.infer.methods.convert]
    [top.kzre.homunculus.core.types.infer.methods.default]
    [top.kzre.homunculus.core.types.infer.methods.define]
    [top.kzre.homunculus.core.types.infer.methods.if]
    [top.kzre.homunculus.core.types.infer.methods.lambda]
    [top.kzre.homunculus.core.types.infer.methods.let]
    [top.kzre.homunculus.core.types.infer.methods.literal]
    [top.kzre.homunculus.core.types.infer.methods.loop]
    [top.kzre.homunculus.core.types.infer.methods.map]
    [top.kzre.homunculus.core.types.infer.methods.member-access]
    [top.kzre.homunculus.core.types.infer.methods.ns]
    [top.kzre.homunculus.core.types.infer.methods.protocol]
    [top.kzre.homunculus.core.types.infer.methods.record]
    [top.kzre.homunculus.core.types.infer.methods.recur]      ;; 包含 try / catch / throw
    [top.kzre.homunculus.core.types.infer.methods.try]
    [top.kzre.homunculus.core.types.infer.methods.variable]
    [top.kzre.homunculus.core.types.infer.methods.vector]
    [top.kzre.homunculus.core.types.infer.methods.while]))

(def make-context core/make-context)

;; re-export 核心入口
(def infer c/infer)
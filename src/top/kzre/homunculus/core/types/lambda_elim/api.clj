(ns top.kzre.homunculus.core.types.lambda-elim.api
  "闭包消除 Pass 的公共入口。加载所有 defmethod 并重导出核心函数。"
  (:require
    [top.kzre.homunculus.core.types.lambda-elim.core :as core]
    ;; 所有节点类型的方法实现
    [top.kzre.homunculus.core.types.lambda-elim.methods.call]
    [top.kzre.homunculus.core.types.lambda-elim.methods.let]
    [top.kzre.homunculus.core.types.lambda-elim.methods.lambda]
    [top.kzre.homunculus.core.types.lambda-elim.methods.block]
    [top.kzre.homunculus.core.types.lambda-elim.methods.if]
    [top.kzre.homunculus.core.types.lambda-elim.methods.loop]
    [top.kzre.homunculus.core.types.lambda-elim.methods.define]
    [top.kzre.homunculus.core.types.lambda-elim.methods.while]
    [top.kzre.homunculus.core.types.lambda-elim.methods.try]
    [top.kzre.homunculus.core.types.lambda-elim.methods.catch]
    [top.kzre.homunculus.core.types.lambda-elim.methods.throw]
    [top.kzre.homunculus.core.types.lambda-elim.methods.assign]
    [top.kzre.homunculus.core.types.lambda-elim.methods.vector]
    [top.kzre.homunculus.core.types.lambda-elim.methods.map]
    [top.kzre.homunculus.core.types.lambda-elim.methods.literal]
    [top.kzre.homunculus.core.types.lambda-elim.methods.variable]
    [top.kzre.homunculus.core.types.lambda-elim.methods.recur]
    [top.kzre.homunculus.core.types.lambda-elim.methods.convert]
    [top.kzre.homunculus.core.types.lambda-elim.methods.ns]
    [top.kzre.homunculus.core.types.lambda-elim.methods.record]
    [top.kzre.homunculus.core.types.lambda-elim.methods.protocol]
    [top.kzre.homunculus.core.types.lambda-elim.methods.member-access]
    [top.kzre.homunculus.core.types.lambda-elim.methods.array]))

;; 闭包消除pass，这里是最后一个pass，对剩余所有闭包执行单态化提升
(def eliminate core/elaborate)
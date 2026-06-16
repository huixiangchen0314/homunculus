(ns top.kzre.homunculus.core.types.lambda-inline.api
  "Lambda 内联 Pass 的公共 API。"
  (:require
    [top.kzre.homunculus.core.types.lambda-inline.core :as core]
    ;; 加载所有方法实现
    [top.kzre.homunculus.core.types.lambda-inline.methods.assign]
    [top.kzre.homunculus.core.types.lambda-inline.methods.block]
    [top.kzre.homunculus.core.types.lambda-inline.methods.call]
    [top.kzre.homunculus.core.types.lambda-inline.methods.catch]
    [top.kzre.homunculus.core.types.lambda-inline.methods.convert]
    [top.kzre.homunculus.core.types.lambda-inline.methods.default]
    [top.kzre.homunculus.core.types.lambda-inline.methods.define]
    [top.kzre.homunculus.core.types.lambda-inline.methods.if]
    [top.kzre.homunculus.core.types.lambda-inline.methods.lambda]
    [top.kzre.homunculus.core.types.lambda-inline.methods.let]
    [top.kzre.homunculus.core.types.lambda-inline.methods.literal]
    [top.kzre.homunculus.core.types.lambda-inline.methods.loop]
    [top.kzre.homunculus.core.types.lambda-inline.methods.map]
    [top.kzre.homunculus.core.types.lambda-inline.methods.member-access]
    [top.kzre.homunculus.core.types.lambda-inline.methods.ns]
    [top.kzre.homunculus.core.types.lambda-inline.methods.protocol]
    [top.kzre.homunculus.core.types.lambda-inline.methods.record]
    [top.kzre.homunculus.core.types.lambda-inline.methods.recur]
    [top.kzre.homunculus.core.types.lambda-inline.methods.throw]
    [top.kzre.homunculus.core.types.lambda-inline.methods.try]
    [top.kzre.homunculus.core.types.lambda-inline.methods.variable]
    [top.kzre.homunculus.core.types.lambda-inline.methods.vector]
    [top.kzre.homunculus.core.types.lambda-inline.methods.while]))

(def eliminate-inline core/eliminate-inline)
(def inline-pass core/inline-pass)
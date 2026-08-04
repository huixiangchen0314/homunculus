(ns top.kzre.homunculus.backend.hlsl.api
  "HLSL 代码生成公共入口。加载所有发射方法，提供上下文构造与发射函数。"
  (:require
    [top.kzre.homunculus.backend.hlsl.backend :as backend]
    ;; 加载各方法文件以注册 defmethod
    [top.kzre.homunculus.backend.hlsl.emitter :as emitter]
    [top.kzre.homunculus.backend.hlsl.frontend :as frontend]
    [top.kzre.homunculus.backend.hlsl.methods.array]
    [top.kzre.homunculus.backend.hlsl.methods.assign]
    [top.kzre.homunculus.backend.hlsl.methods.block]
    [top.kzre.homunculus.backend.hlsl.methods.call]
    [top.kzre.homunculus.backend.hlsl.methods.convert]
    [top.kzre.homunculus.backend.hlsl.methods.define]
    [top.kzre.homunculus.backend.hlsl.methods.if]
    [top.kzre.homunculus.backend.hlsl.methods.lambda]
    [top.kzre.homunculus.backend.hlsl.methods.let]
    [top.kzre.homunculus.backend.hlsl.methods.literal]
    [top.kzre.homunculus.backend.hlsl.methods.member-access]
    [top.kzre.homunculus.backend.hlsl.methods.ns]
    [top.kzre.homunculus.backend.hlsl.methods.record]
    [top.kzre.homunculus.backend.hlsl.methods.variable]
    [top.kzre.homunculus.backend.hlsl.methods.vector]
    [top.kzre.homunculus.backend.hlsl.methods.while]
    [top.kzre.homunculus.compilers.firstorder :as firstorder]
    [top.kzre.homunculus.internal.model :as model]))


(defonce hlsl-target (model/make-compile-target
                      frontend/frontend backend/backend
                      firstorder/compiler
                      emitter/emitter))
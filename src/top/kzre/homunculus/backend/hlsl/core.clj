(ns top.kzre.homunculus.backend.hlsl.core
  "HLSL 代码生成公共入口。加载所有发射方法，提供上下文构造与发射函数。"
  (:require
    [top.kzre.homunculus.backend.hlsl.backend :as backend]
    [top.kzre.homunculus.backend.hlsl.emitter :as emitter]
    [top.kzre.homunculus.backend.hlsl.frontend :as frontend]
    [top.kzre.homunculus.compilers.firstorder :as firstorder]
    [top.kzre.homunculus.internal.model :as model]))


(defonce hlsl-target (model/make-compile-target
                      frontend/frontend backend/backend
                      firstorder/compiler
                      emitter/emitter))
(ns top.kzre.homunculus.backend.hlsl.api
  "HLSL 代码生成公共入口，加载所有发射方法。"
  (:require
    [top.kzre.homunculus.backend.hlsl.core :as core]
    [top.kzre.homunculus.backend.hlsl.methods.literal]
    [top.kzre.homunculus.backend.hlsl.methods.variable]
    [top.kzre.homunculus.backend.hlsl.methods.call]
    [top.kzre.homunculus.backend.hlsl.methods.if]
    [top.kzre.homunculus.backend.hlsl.methods.block]
    [top.kzre.homunculus.backend.hlsl.methods.while]
    [top.kzre.homunculus.backend.hlsl.methods.assign]
    [top.kzre.homunculus.backend.hlsl.methods.let]
    [top.kzre.homunculus.backend.hlsl.methods.convert]
    [top.kzre.homunculus.backend.hlsl.methods.member-access]
    [top.kzre.homunculus.backend.hlsl.methods.record]
    [top.kzre.homunculus.backend.hlsl.methods.vector]
    [top.kzre.homunculus.backend.hlsl.methods.lambda]
    [top.kzre.homunculus.backend.hlsl.methods.define]
    [top.kzre.homunculus.backend.hlsl.methods.resource]))

(def emit core/emit)
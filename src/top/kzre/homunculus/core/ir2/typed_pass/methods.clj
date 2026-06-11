(ns top.kzre.homunculus.core.ir2.typed-pass.methods
  "加载所有 typed-pass 方法实现，注册到 core/infer multimethod。"
  (:require [top.kzre.homunculus.core.ir2.typed-pass.methods.literal]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.variable]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.call]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.if]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.block]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.let]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.lambda]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.loop]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.define]
            [top.kzre.homunculus.core.ir2.typed-pass.methods.default]))
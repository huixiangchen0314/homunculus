(ns top.kzre.homunculus.core.types.typed.methods
  (:require [top.kzre.homunculus.core.types.typed.methods.literal]
            [top.kzre.homunculus.core.types.typed.methods.variable]
            [top.kzre.homunculus.core.types.typed.methods.call]
            [top.kzre.homunculus.core.types.typed.methods.if]
            [top.kzre.homunculus.core.types.typed.methods.block]
            [top.kzre.homunculus.core.types.typed.methods.let]
            [top.kzre.homunculus.core.types.typed.methods.lambda]
            [top.kzre.homunculus.core.types.typed.methods.loop]          ;; 包含 :loop 和 :recur
            [top.kzre.homunculus.core.types.typed.methods.define]
            [top.kzre.homunculus.core.types.typed.methods.vector]
            [top.kzre.homunculus.core.types.typed.methods.map]
            [top.kzre.homunculus.core.types.typed.methods.try]           ;; 包含 :try, :catch, :throw
            [top.kzre.homunculus.core.types.typed.methods.assign]
            [top.kzre.homunculus.core.types.typed.methods.default]))
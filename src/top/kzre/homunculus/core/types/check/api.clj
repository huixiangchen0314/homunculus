(ns top.kzre.homunculus.core.types.check.api
  (:require [top.kzre.homunculus.core.types.check.core]
            [top.kzre.homunculus.core.types.check.methods.literal]
            [top.kzre.homunculus.core.types.check.methods.variable]
            [top.kzre.homunculus.core.types.check.methods.call]
            [top.kzre.homunculus.core.types.check.methods.if]
            [top.kzre.homunculus.core.types.check.methods.block]
            [top.kzre.homunculus.core.types.check.methods.let]
            [top.kzre.homunculus.core.types.check.methods.lambda]
            [top.kzre.homunculus.core.types.check.methods.loop]      ;; 含 :loop 和 :recur
            [top.kzre.homunculus.core.types.check.methods.define]
            [top.kzre.homunculus.core.types.check.methods.vector]
            [top.kzre.homunculus.core.types.check.methods.map]
            [top.kzre.homunculus.core.types.check.methods.while]
            [top.kzre.homunculus.core.types.check.methods.try]       ;; 含 :try, :catch, :throw
            [top.kzre.homunculus.core.types.check.methods.assign]
            [top.kzre.homunculus.core.types.check.methods.default]))

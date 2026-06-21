(ns top.kzre.homunculus.core.types.check.api
  (:require [top.kzre.homunculus.core.types.check.core :as core]
            [top.kzre.homunculus.core.types.check.methods.assign]
            [top.kzre.homunculus.core.types.check.methods.block]
            [top.kzre.homunculus.core.types.check.methods.call]
            [top.kzre.homunculus.core.types.check.methods.convert]
            [top.kzre.homunculus.core.types.check.methods.default]
            [top.kzre.homunculus.core.types.check.methods.define]
            [top.kzre.homunculus.core.types.check.methods.if]
            [top.kzre.homunculus.core.types.check.methods.lambda]
            [top.kzre.homunculus.core.types.check.methods.let]
            [top.kzre.homunculus.core.types.check.methods.literal]
            [top.kzre.homunculus.core.types.check.methods.loop]
            [top.kzre.homunculus.core.types.check.methods.map]
            [top.kzre.homunculus.core.types.check.methods.member-access]
            [top.kzre.homunculus.core.types.check.methods.ns]
            [top.kzre.homunculus.core.types.check.methods.protocol]
            [top.kzre.homunculus.core.types.check.methods.record]
            [top.kzre.homunculus.core.types.check.methods.try]
            [top.kzre.homunculus.core.types.check.methods.variable]
            [top.kzre.homunculus.core.types.check.methods.vector]
            [top.kzre.homunculus.core.types.check.methods.array]
            [top.kzre.homunculus.core.types.check.methods.while]))

(def make-context core/make-context)

(def check core/check-program)
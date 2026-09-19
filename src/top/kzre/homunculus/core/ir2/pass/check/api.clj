(ns top.kzre.homunculus.core.ir2.pass.check.api
  (:require [top.kzre.homunculus.core.ir2.pass.check.core :as core]
            [top.kzre.homunculus.core.ir2.pass.check.methods.assign]
            [top.kzre.homunculus.core.ir2.pass.check.methods.block]
            [top.kzre.homunculus.core.ir2.pass.check.methods.call]
            [top.kzre.homunculus.core.ir2.pass.check.methods.convert]
            [top.kzre.homunculus.core.ir2.pass.check.methods.default]
            [top.kzre.homunculus.core.ir2.pass.check.methods.define]
            [top.kzre.homunculus.core.ir2.pass.check.methods.if]
            [top.kzre.homunculus.core.ir2.pass.check.methods.lambda]
            [top.kzre.homunculus.core.ir2.pass.check.methods.let]
            [top.kzre.homunculus.core.ir2.pass.check.methods.literal]
            [top.kzre.homunculus.core.ir2.pass.check.methods.loop]
            [top.kzre.homunculus.core.ir2.pass.check.methods.map]
            [top.kzre.homunculus.core.ir2.pass.check.methods.member-access]
            [top.kzre.homunculus.core.ir2.pass.check.methods.ns]
            [top.kzre.homunculus.core.ir2.pass.check.methods.protocol]
            [top.kzre.homunculus.core.ir2.pass.check.methods.record]
            [top.kzre.homunculus.core.ir2.pass.check.methods.try]
            [top.kzre.homunculus.core.ir2.pass.check.methods.variable]
            [top.kzre.homunculus.core.ir2.pass.check.methods.vector]
            [top.kzre.homunculus.core.ir2.pass.check.methods.array]
            [top.kzre.homunculus.core.ir2.pass.check.methods.while]))

(def make-context core/make-context)

(def check core/check-program)
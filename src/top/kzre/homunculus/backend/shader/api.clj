(ns top.kzre.homunculus.backend.shader.api
  (:require [top.kzre.homunculus.backend.shader.emit :as e]
            [top.kzre.homunculus.backend.shader.core :as core]
            [top.kzre.homunculus.backend.shader.methods.literal]
            [top.kzre.homunculus.backend.shader.methods.variable]
            [top.kzre.homunculus.backend.shader.methods.call]
            [top.kzre.homunculus.backend.shader.methods.if]
            [top.kzre.homunculus.backend.shader.methods.while]
            [top.kzre.homunculus.backend.shader.methods.block]
            [top.kzre.homunculus.backend.shader.methods.let]
            [top.kzre.homunculus.backend.shader.methods.assign]
            [top.kzre.homunculus.backend.shader.methods.define]
            [top.kzre.homunculus.backend.shader.methods.lambda]
            [top.kzre.homunculus.backend.shader.methods.loop]
            [top.kzre.homunculus.backend.shader.methods.recur]
            [top.kzre.homunculus.backend.shader.methods.try]
            [top.kzre.homunculus.backend.shader.methods.catch]
            [top.kzre.homunculus.backend.shader.methods.throw]
            [top.kzre.homunculus.backend.shader.methods.vector]
            [top.kzre.homunculus.backend.shader.methods.map]
            [top.kzre.homunculus.backend.shader.methods.convert]))

(def emit core/emit)
(ns top.kzre.homunculus.core.types.constraint.api
  "约束生成与求解的公共入口。
   负责加载所有 gen 方法实现并 re-export 对外函数。"
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
    ;; 加载所有 defmethod 实现，触发注册
            [top.kzre.homunculus.core.types.constraint.gen.methods.literal]
            [top.kzre.homunculus.core.types.constraint.gen.methods.variable]
            [top.kzre.homunculus.core.types.constraint.gen.methods.call]
            [top.kzre.homunculus.core.types.constraint.gen.methods.let]
            [top.kzre.homunculus.core.types.constraint.gen.methods.loop]
            [top.kzre.homunculus.core.types.constraint.gen.methods.block]
            [top.kzre.homunculus.core.types.constraint.gen.methods.if]
            [top.kzre.homunculus.core.types.constraint.gen.methods.define]
            [top.kzre.homunculus.core.types.constraint.gen.methods.lambda]
            [top.kzre.homunculus.core.types.constraint.gen.methods.assign]
            [top.kzre.homunculus.core.types.constraint.gen.methods.while]
            [top.kzre.homunculus.core.types.constraint.gen.methods.vector]
            [top.kzre.homunculus.core.types.constraint.gen.methods.map]
            [top.kzre.homunculus.core.types.constraint.gen.methods.try]))

;; re-export 关键函数，外部只需依赖此 api 即可
(def process solve/process)
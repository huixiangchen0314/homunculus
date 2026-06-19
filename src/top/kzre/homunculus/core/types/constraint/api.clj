(ns top.kzre.homunculus.core.types.constraint.api
  "约束系统的公共入口：加载所有 gen 方法实现，并 re-export 关键函数。"
  (:require
    ;; 加载各 gen 方法以注册多方法（defmethod）
    [top.kzre.homunculus.core.types.constraint.gen.methods.assign]
    [top.kzre.homunculus.core.types.constraint.gen.methods.block]
    [top.kzre.homunculus.core.types.constraint.gen.methods.call]
    [top.kzre.homunculus.core.types.constraint.gen.methods.define]
    [top.kzre.homunculus.core.types.constraint.gen.methods.if]
    [top.kzre.homunculus.core.types.constraint.gen.methods.lambda]
    [top.kzre.homunculus.core.types.constraint.gen.methods.let]
    [top.kzre.homunculus.core.types.constraint.gen.methods.literal]
    [top.kzre.homunculus.core.types.constraint.gen.methods.loop]
    [top.kzre.homunculus.core.types.constraint.gen.methods.map]
    [top.kzre.homunculus.core.types.constraint.gen.methods.try]
    [top.kzre.homunculus.core.types.constraint.gen.methods.variable]
    [top.kzre.homunculus.core.types.constraint.gen.methods.vector]
    [top.kzre.homunculus.core.types.constraint.gen.methods.while]
    ;; 从约束核心入口导入所需函数
    [top.kzre.homunculus.core.types.constraint.core :as core]))

;; re-export 关键函数，外部只需依赖此 api 即可
(def make-context core/make-context)
(def process core/process)
(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.api
  "闭包消除 Pass 的公共入口。加载所有 defmethod 并重导出核心函数。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.env]
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.core]
    ;; 所有节点类型的方法实现
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.call]
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.let]
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.lambda]
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.loop]
    [top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.define]
    [top.kzre.krro.core.util.re-export :refer [re-export]]))


(re-export
  [top.kzre.homunculus.core.ir2.pass.lambda-elim.core
   :refer
   [elim]])

(re-export
  [top.kzre.homunculus.core.ir2.pass.lambda-elim.env
   :refer
   [make-env]])
(ns top.kzre.homunculus.core.ir1.api
  "IR1 的公共入口：加载所有特殊形式处理方法，并导出核心转换函数。"
        (:require [top.kzre.homunculus.core.ir1.core]
                  [top.kzre.homunculus.core.ir1.preprocess]
                ;; 基础字面量
                  [top.kzre.homunculus.core.ir1.forms.literal]
                  [top.kzre.homunculus.core.ir1.forms.symbol]
                  [top.kzre.homunculus.core.ir1.forms.vector]
                  [top.kzre.homunculus.core.ir1.forms.map]
                ;; 控制流
                  [top.kzre.homunculus.core.ir1.forms.if]
                  [top.kzre.homunculus.core.ir1.forms.do]
                ;; 绑定
                  [top.kzre.homunculus.core.ir1.forms.let]
                  [top.kzre.homunculus.core.ir1.forms.fn]
                  [top.kzre.homunculus.core.ir1.forms.loop] ;; loop + recur
                ;; 引用 / 赋值
                  [top.kzre.homunculus.core.ir1.forms.set!]
                ;; 异常
                  [top.kzre.homunculus.core.ir1.forms.try]  ;; try / catch / throw
                ;; 命名空间
                  [top.kzre.homunculus.core.ir1.forms.ns]   ;; ns* 特殊形式
                ;; 记录 / 协议 / 成员访问
                  [top.kzre.homunculus.core.ir1.forms.record]
                  [top.kzre.homunculus.core.ir1.forms.protocol]
                  [top.kzre.homunculus.core.ir1.forms.member-access]
                  [top.kzre.krro.core.util.re-export :refer [re-export]]))

(re-export
        [top.kzre.homunculus.core.ir1.core
         :refer [lower make-env]]
        [top.kzre.homunculus.core.ir1.preprocess
         :refer [preprocess]])


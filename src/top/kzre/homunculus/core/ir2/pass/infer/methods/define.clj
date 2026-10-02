(ns top.kzre.homunculus.core.ir2.pass.infer.methods.define
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod infer/infer-node* :define [node env]
  ;; 1. 对 :define 节点的值表达式进行局部推导
  (if (true? (:ho? (n/attrs node)))
    (infer/nothing node env)
    (let [[val-type val-node val-env] (infer/infer-node* (n/define-val node) env)]
      ;; 2. 成功条件：值表达式有推导出的类型
      (if val-type
        ;; —— 成功路径 ——
        ;; 将推导后的值节点放回，并标注整个 define 节点的类型为该值类型
        (let [name     (n/define-name node)
              new-node (-> node
                           (n/define-with-val val-node)
                           (t/set-type! val-type))
              ;; 将定义名与类型写入环境
              new-env  (p/bind-var val-env name val-type)]
          (infer/success val-type new-node new-env))
        ;; —— 失败路径 ——
        ;; 仍保留子节点的推导信息，但不标注本节点类型
        (infer/nothing (n/define-with-val node val-node) val-env)))))
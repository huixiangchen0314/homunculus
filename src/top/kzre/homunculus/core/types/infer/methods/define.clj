(ns top.kzre.homunculus.core.types.infer.methods.define
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]
            [top.kzre.homunculus.core.types.env :as e]))

(defmethod infer/local-infer :define [node context]
  ;; 1. 对 :define 节点的值表达式进行局部推导
  (let [[val-ty val-node val-ctx] (infer/local-infer (n/define-val node) context)]
    ;; 2. 成功条件：值表达式有推导出的类型 (val-ty 非 nil)
    (if val-ty
      ;; —— 成功路径 ——
      ;; 将推导后的值节点放回，并标注整个 define 节点的类型为该值类型
      (let [name (n/define-name node)
            new-node (-> node (n/define-with-val val-node) (t/set-type! val-ty))
            ;; 将定义名称与类型写入环境
            new-env (e/extend-env (infer/env val-ctx) name val-ty)]
        (infer/success val-ty new-node (infer/new-env val-ctx new-env)))
      ;; —— 失败路径 ——
      ;; 仍保留子节点的推导信息，但不标注本节点类型
      (infer/nothing (n/define-with-val node val-node) val-ctx))))
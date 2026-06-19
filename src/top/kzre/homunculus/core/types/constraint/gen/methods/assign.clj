(ns top.kzre.homunculus.core.types.constraint.gen.methods.assign
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :assign [node context]
  ;; 1. 推导左侧变量，获取新上下文
  (let [[var-tv var-node var-constr var-ctx] (gen/cg-node-raw (n/assign-var node) context)
        ;; 2. 推导右侧值，使用变量推导后的上下文
        [val-tv val-node val-constr val-ctx] (gen/cg-node-raw (n/assign-val node) var-ctx)
        ;; 赋值表达式类型与左右两侧一致（通过约束相等）
        all-constr (concat var-constr val-constr
                           (when (and var-tv val-tv)
                             [(c/make-cequal var-tv val-tv)]))]
    [var-tv
     (t/set-type! (n/make-assign var-node val-node
                                  (n/attrs node) (n/node-meta node) (n/parent node))
                   var-tv)
     all-constr
     val-ctx]))
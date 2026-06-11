(ns top.kzre.homunculus.core.ir2.forms.variable
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]))

(defmethod ir2/lower-ast :var [ir1-vec env]
  ;; IR1 :var 特殊形式: (var foo) → 直接转为对 foo 的引用（即 :variable 节点）
  ;; 因为 lowering :symbol 已经产生 :variable，所以直接取 lowered 结果即可
  (let [sym-ir (second ir1-vec)
        sym    (first (ir2/lower-ast sym-ir env))]
    [sym]))
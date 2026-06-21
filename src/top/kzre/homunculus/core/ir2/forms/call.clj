(ns top.kzre.homunculus.core.ir2.forms.call
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]
            [top.kzre.homunculus.core.ir2.model :as m]))

;; ── 特殊的数组操作符号 ──────────────────────
(def ^:private special-array-ops
  #{'%%aget '%%aset '%%new-array '%%alength '%%aslice})

(defmethod ir2/lower-ast :call [node env]
  (let [fn-node (first (ir2/lower-ast (n1/call-op node) env))
        args    (mapv #(first (ir2/lower-ast % env)) (n1/call-args node))]
    (if (and (n2/variable-node? fn-node)                     ;; 操作符是变量节点
             (contains? special-array-ops (n2/var-name fn-node))) ;; 且属于特殊数组操作
      ;; ── 分流到特殊 IR2 节点 ──
      (let [op (n2/var-name fn-node)]
        (case op
          %%aget
          (let [[target idx] args]
            [(m/->AGetNode target idx (n1/node-meta node) nil)])
          %%aset
          (let [[target idx val] args]
            [(m/->ASetNode target idx val (n1/node-meta node) nil)])
          %%new-array
          (let [[size] args]
            [(m/->NewArrayNode size (n1/node-meta node) nil)])
          %%alength
          (let [[target] args]
            [(m/->ALengthNode target (n1/node-meta node) nil)])
          %%aslice
          (let [[target start end] args]
            [(m/->ASliceNode target start end (n1/node-meta node) nil)])))
      ;; ── 普通调用 ──
      [(n2/make-call fn-node args {} (n1/node-meta node) nil)])))
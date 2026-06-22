(ns top.kzre.homunculus.backend.hlsl.methods.array
  "HLSL 数组特殊节点发射。"
  (:require [top.kzre.homunculus.backend.hlsl.core :as core]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod core/emit-node :new-array [node context]
  (let [size (core/emit-node (n/new-array-size node) context)]
    [:new-array size]))

(defmethod core/emit-node :aget [node context]
  [:aget (core/emit-node (n/aget-target node) context)
   (core/emit-node (n/aget-idx node) context)])

(defmethod core/emit-node :aset [node context]
  (let [target-ast (core/emit-node (n/aset-target node) context)
        idx-ast    (core/emit-node (n/aset-idx node) context)
        val-ast    (core/emit-node (n/aset-val node) context)]
    (if (and (vector? val-ast) (not (keyword? (first val-ast))))
      ;; 值是一个无标签向量（语句序列），提取最后一个表达式作为值，
      ;; 前置语句放入结果向量的前面
      (let [val-expr (last val-ast)
            pre-stmts (butlast val-ast)]
        (vec (concat pre-stmts [[:aset target-ast idx-ast val-expr]])))
      ;; 正常单一表达式
      [:aset target-ast idx-ast val-ast])))

(defmethod core/emit-node :alength [node context]
  (let [target-ty (ty/get-type (n/alength-target node) (:known-types context))]
    (if-let [len (when (ty/vec-type? target-ty) (ty/vec-size target-ty))]
      (if (integer? len)
        [:literal len]
        (throw (ex-info "Array length is not a compile-time constant" {})))
      (throw (ex-info "Cannot determine array length" {})))))
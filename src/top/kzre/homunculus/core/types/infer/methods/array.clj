(ns top.kzre.homunculus.core.types.infer.methods.array
  "数组特殊节点的局部类型推导。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.core.types.protocol :as tp]))

(defmethod infer/local-infer :new-array [node context]
  (let [[size-ty size-node size-ctx] (infer/local-infer (n/new-array-size node) context)
        backend (infer/backend context)
        hetero? (when backend (tp/support-hetero-vec backend))]
    (if hetero?
      ;; 异构向量：初始类型为空，无法确定元素，直接 nothing
      (infer/nothing (n/make-new-array size-node (n/node-meta node) (n/parent node)) size-ctx)
      ;; 同构向量：无法确定元素类型，nothing
      (infer/nothing (n/make-new-array size-node (n/node-meta node) (n/parent node)) size-ctx))))

(defmethod infer/local-infer :aget [node context]
  (let [[target-ty target-node target-ctx] (infer/local-infer (n/aget-target node) context)
        [idx-ty idx-node idx-ctx] (infer/local-infer (n/aget-idx node) target-ctx)
        new-node (n/make-aget target-node idx-node (n/node-meta node) (n/parent node))]
    (if-let [elem-ty (cond
                       (ty/vec-type? target-ty)
                       (ty/vec-element-type target-ty)
                       (ty/hetero-vec? target-ty)
                       (let [idx-val (when (n/literal-node? idx-node) (n/lit-val idx-node))]
                         (when idx-val
                           (nth (ty/hetero-vec-types target-ty) idx-val nil)))
                       :else nil)]
      (infer/success elem-ty (ty/set-type! new-node elem-ty) idx-ctx)
      (infer/nothing new-node idx-ctx))))

(defmethod infer/local-infer :aset [node context]
  (let [[target-ty target-node target-ctx] (infer/local-infer (n/aset-target node) context)
        [idx-ty idx-node idx-ctx] (infer/local-infer (n/aset-idx node) target-ctx)
        [val-ty val-node val-ctx] (infer/local-infer (n/aset-val node) idx-ctx)
        new-node (n/make-aset target-node idx-node val-node (n/node-meta node) (n/parent node))]
    ;; aset 无返回值
    (infer/nothing new-node val-ctx)))

(defmethod infer/local-infer :alength [node context]
  (let [[target-ty target-node target-ctx] (infer/local-infer (n/alength-target node) context)
        int-ty (ty/make-tcon (tp/integer-type (infer/frontend context)))
        new-node (n/make-alength target-node (n/node-meta node) (n/parent node))]
    (infer/success int-ty (ty/set-type! new-node int-ty) target-ctx)))

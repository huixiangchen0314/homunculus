(ns top.kzre.homunculus.core.ir2.pass.infer.methods.array
  "数组特殊节点的局部类型推导。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod infer/infer-node* :new-array [node env]
  (let [[_ size-node size-env] (infer/infer-node* (n/new-array-size node) env)
        new-node (n/make-new-array size-node
                                   (n/attrs node)
                                   (n/node-meta node))]
    (if (p/use-hetero-vec? env)
      (infer/nothing new-node size-env)
      (infer/nothing new-node size-env))))

(defmethod infer/infer-node* :aget [node env]
  (let [[target-ty target-node target-env] (infer/infer-node* (n/aget-target node) env)
        [_ idx-node idx-env]                (infer/infer-node* (n/aget-idx node) target-env)
        new-node (n/make-aget target-node idx-node
                              (n/attrs node)
                              (n/node-meta node))]
    (if-let [elem-ty (cond
                       (ty/vec-type? target-ty)
                       (ty/vec-element-type target-ty)
                       (ty/hetero-vec? target-ty)
                       (let [idx-val (when (n/literal-node? idx-node) (n/lit-val idx-node))]
                         (when idx-val
                           (nth (ty/hetero-vec-types target-ty) idx-val nil)))
                       :else nil)]
      (infer/success elem-ty (ty/set-type! new-node elem-ty) idx-env)
      (infer/nothing new-node idx-env))))

(defmethod infer/infer-node* :aset [node env]
  (let [[_ target-node target-env] (infer/infer-node* (n/aset-target node) env)
        [_ idx-node idx-env]       (infer/infer-node* (n/aset-idx node) target-env)
        [_ val-node val-env]       (infer/infer-node* (n/aset-val node) idx-env)
        new-node (n/make-aset target-node idx-node val-node
                              (n/attrs node)
                              (n/node-meta node))]
    (infer/nothing new-node val-env)))

(defmethod infer/infer-node* :alength [node env]
  (let [[_ target-node target-env] (infer/infer-node* (n/alength-target node) env)
        int-ty   (ty/make-tcon (p/integer-type env))
        new-node (n/make-alength target-node
                                 (n/attrs node)
                                 (n/node-meta node))]
    (infer/success int-ty (ty/set-type! new-node int-ty) target-env)))
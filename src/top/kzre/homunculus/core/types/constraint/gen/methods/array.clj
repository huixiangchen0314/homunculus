(ns top.kzre.homunculus.core.types.constraint.gen.methods.array
  "数组特殊节点的约束生成。"
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]
            [top.kzre.homunculus.core.types.protocol :as tp]))

;; ── new-array ──────────────────────────────
(defmethod gen/cg-node-raw :new-array [node context]
  (let [[size-tv size-node size-constrs size-ctx] (gen/cg-node-raw (n/new-array-size node) context)
        int-ty   (ty/make-tcon (tp/integer-type (u/frontend context)))
        ;; ★ 约束 size 的类型为 int
        size-eq  (c/make-cequal size-tv int-ty)
        backend  (u/backend context)
        hetero?  (when backend (tp/support-hetero-vec backend))]
    (if hetero?
      (let [tv (ty/make-hetero-vec [])
            new-node (n/make-new-array size-node (n/node-meta node) (n/parent node))]
        [tv (ty/set-type! new-node tv) (concat size-constrs [size-eq]) size-ctx])
      (let [;; 长度：若 size 是字面量整数则直接提取值，否则使用 size-tv 作为长度变量
            len      (if (and (n/literal-node? size-node) (integer? (n/lit-val size-node)))
                       (n/lit-val size-node)
                       size-tv)   ;; 使用与 size 关联的类型变量
            elem-tv  (gen/fresh-tvar)
            tv       (ty/make-tvec elem-tv len)
            new-node (n/make-new-array size-node (n/node-meta node) (n/parent node))]
        [tv (ty/set-type! new-node tv) (concat size-constrs [size-eq]) size-ctx]))))


;; ── aget ───────────────────────────────────
(defmethod gen/cg-node-raw :aget [node context]
  (let [[target-tv target-node target-constrs target-ctx] (gen/cg-node-raw (n/aget-target node) context)
        [idx-tv idx-node idx-constrs idx-ctx] (gen/cg-node-raw (n/aget-idx node) target-ctx)
        ;; 确定元素类型
        elem-tv (cond
                  (ty/vec-type? target-tv)
                  (ty/vec-element-type target-tv)
                  (ty/hetero-vec? target-tv)
                  (let [idx-val (when (n/literal-node? idx-node) (n/lit-val idx-node))]
                    (when idx-val (nth (ty/hetero-vec-types target-tv) idx-val nil)))
                  :else (gen/fresh-tvar))
        ;; 若 target-tv 仍为未知变量，则强制为同构向量
        len-tv (cond
                 (ty/vec-type? target-tv) (ty/vec-size target-tv)
                 :else (gen/fresh-tvar))
        vec-tv (ty/make-tvec elem-tv len-tv)
        target-eq (when (and (not (ty/vec-type? target-tv))
                             (not (ty/hetero-vec? target-tv)))
                    (c/make-cequal target-tv vec-tv))
        new-node (n/make-aget target-node idx-node (n/node-meta node) (n/parent node))
        constrs (concat target-constrs idx-constrs (when target-eq [target-eq]))]
    [elem-tv (ty/set-type! new-node elem-tv) constrs idx-ctx]))

;; ── aset ───────────────────────────────────
(defmethod gen/cg-node-raw :aset [node context]
  (let [[target-tv target-node target-constrs target-ctx] (gen/cg-node-raw (n/aset-target node) context)
        [idx-tv idx-node idx-constrs idx-ctx] (gen/cg-node-raw (n/aset-idx node) target-ctx)
        [val-tv val-node val-constrs val-ctx] (gen/cg-node-raw (n/aset-val node) idx-ctx)
        ;; 确定该位置的元素类型
        elem-tv (cond
                  (ty/vec-type? target-tv)
                  (ty/vec-element-type target-tv)
                  (ty/hetero-vec? target-tv)
                  (let [idx-val (when (n/literal-node? idx-node) (n/lit-val idx-node))]
                    (when idx-val (nth (ty/hetero-vec-types target-tv) idx-val nil)))
                  :else (gen/fresh-tvar))
        len-tv (cond
                 (ty/vec-type? target-tv) (ty/vec-size target-tv)
                 :else (gen/fresh-tvar))
        vec-tv (ty/make-tvec elem-tv len-tv)
        target-eq (when (and (not (ty/vec-type? target-tv))
                             (not (ty/hetero-vec? target-tv)))
                    (c/make-cequal target-tv vec-tv))
        val-eq (c/make-cequal val-tv elem-tv)
        new-node (n/make-aset target-node idx-node val-node (n/node-meta node) (n/parent node))
        constrs (concat target-constrs idx-constrs val-constrs
                        (when target-eq [target-eq]) [val-eq])]
    [nil (ty/set-type! new-node nil) constrs val-ctx]))

;; ── alength ────────────────────────────────
(defmethod gen/cg-node-raw :alength [node context]
  (let [[target-tv target-node target-constrs target-ctx] (gen/cg-node-raw (n/alength-target node) context)
        int-ty (ty/make-tcon (tp/integer-type (u/frontend context)))]
    (if (and (ty/vec-type? target-tv) (integer? (ty/vec-size target-tv)))
      ;; 长度已知，直接返回整数字面量节点
      (let [len-val (ty/vec-size target-tv)
            lit-node (n/make-literal len-val nil nil)]
        [int-ty (ty/set-type! lit-node int-ty) target-constrs target-ctx])
      ;; 长度未知，保留原调用
      (let [new-node (n/make-alength target-node (n/node-meta node) (n/parent node))]
        [int-ty (ty/set-type! new-node int-ty) target-constrs target-ctx]))))

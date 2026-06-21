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
        backend (u/backend context)
        hetero? (when backend (tp/support-hetero-vec backend))]
    (if hetero?
      (let [tv (ty/make-hetero-vec [])
            new-node (n/make-new-array size-node (n/node-meta node) (n/parent node))]
        [tv (ty/set-type! new-node tv) size-constrs size-ctx])
      (let [len (if (and (n/literal-node? size-node) (integer? (n/lit-val size-node)))
                  (n/lit-val size-node)
                  (gen/fresh-tvar))
            elem-tv (gen/fresh-tvar)
            tv (ty/make-tvec elem-tv len)
            new-node (n/make-new-array size-node (n/node-meta node) (n/parent node))]
        [tv (ty/set-type! new-node tv) size-constrs size-ctx]))))

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
        int-ty (ty/make-tcon (tp/integer-type (u/frontend context)))
        new-node (n/make-alength target-node (n/node-meta node) (n/parent node))]
    [int-ty (ty/set-type! new-node int-ty) target-constrs target-ctx]))

;; ── aslice ─────────────────────────────────
(defmethod gen/cg-node-raw :aslice [node context]
  (let [[target-tv target-node target-constrs target-ctx] (gen/cg-node-raw (n/aslice-target node) context)
        [start-tv start-node start-constrs start-ctx] (gen/cg-node-raw (n/aslice-start node) target-ctx)
        [end-tv end-node end-constrs end-ctx] (gen/cg-node-raw (n/aslice-end node) start-ctx)
        new-node (n/make-aslice target-node start-node end-node (n/node-meta node) (n/parent node))
        elem-tv (cond
                  (ty/vec-type? target-tv) (ty/vec-element-type target-tv)
                  (ty/hetero-vec? target-tv) (let [start (when (n/literal-node? start-node) (n/lit-val start-node))]
                                               ;; 简化：只取第一个元素的类型
                                               (first (ty/hetero-vec-types target-tv)))
                  :else (gen/fresh-tvar))
        len-tv (cond
                 (ty/vec-type? target-tv) (ty/vec-size target-tv)
                 :else (gen/fresh-tvar))
        vec-tv (ty/make-tvec elem-tv len-tv)
        target-eq (when (and (not (ty/vec-type? target-tv))
                             (not (ty/hetero-vec? target-tv)))
                    (c/make-cequal target-tv vec-tv))
        slice-len-tv (gen/fresh-tvar)
        slice-tv (ty/make-tvec elem-tv slice-len-tv)
        constrs (concat target-constrs start-constrs end-constrs (when target-eq [target-eq]))]
    [slice-tv (ty/set-type! new-node slice-tv) constrs end-ctx]))
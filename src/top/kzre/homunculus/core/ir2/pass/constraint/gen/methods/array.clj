(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.array
  "数组特殊节点的约束生成。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ── new-array ──────────────────────────────
(defmethod gen/gen-node* :new-array [current-node context]
  (let [{size-type        :type
         size-node        :node
         size-constraints :constraints
         size-env         :env}
        (gen/gen-node* (n/new-array-size current-node) context)
        int-ty   (ty/make-tcon (tp/integer-type (u/frontend context)))
        size-eq  (cons/make-cequal size-type int-ty)
        backend  (u/backend context)
        hetero?  (when backend (tp/support-hetero-vec backend))]
    (if hetero?
      (let [tv       (ty/make-hetero-vec [])
            new-node (n/make-new-array size-node (n/node-meta current-node))]
        {:type        tv
         :node        (ty/set-type! new-node tv)
         :constraints (concat size-constraints [size-eq])
         :env         size-env})
      (let [len-type (if (and (n/literal-node? size-node)
                              (integer? (n/lit-val size-node)))
                       (ty/make-tvalue (n/lit-val size-node))
                       (gen/fresh-tvar))
            elem-tv  (gen/fresh-tvar)
            tv       (ty/make-tvec elem-tv len-type)
            new-node (n/make-new-array size-node
                                       (n/attrs current-node)
                                       (n/node-meta current-node))]
        {:type        tv
         :node        (ty/set-type! new-node tv)
         :constraints (concat size-constraints [size-eq])
         :env         size-env}))))

;; ── aget ───────────────────────────────────
(defmethod gen/gen-node* :aget [current-node context]
  (let [{target-type        :type
         target-node        :node
         target-constraints :constraints
         target-env         :env}
        (gen/gen-node* (n/aget-target current-node) context)
        {idx-type        :type
         idx-node        :node
         idx-constraints :constraints
         idx-env         :env}
        (gen/gen-node* (n/aget-idx current-node) target-env)
        elem-tv (cond
                  (ty/vec-type? target-type)
                  (ty/vec-element-type target-type)
                  (ty/hetero-vec? target-type)
                  (let [idx-val (when (n/literal-node? idx-node) (n/lit-val idx-node))]
                    (when idx-val (nth (ty/hetero-vec-types target-type) idx-val nil)))
                  :else (gen/fresh-tvar))
        len-tv  (cond
                  (ty/vec-type? target-type) (ty/vec-size target-type)
                  :else (gen/fresh-tvar))
        vec-tv   (ty/make-tvec elem-tv len-tv)
        new-node (n/make-aget target-node idx-node
                              (n/attrs current-node)
                              (n/node-meta current-node))
        constrs  (concat target-constraints idx-constraints
                         [(cons/make-cequal target-type vec-tv)])]
    {:type        elem-tv
     :node        (ty/set-type! new-node elem-tv)
     :constraints constrs
     :env         idx-env}))

;; ── aset ───────────────────────────────────
(defmethod gen/gen-node* :aset [current-node context]
  (let [{target-type        :type
         target-node        :node
         target-constraints :constraints
         target-env         :env}
        (gen/gen-node* (n/aset-target current-node) context)
        {idx-type        :type
         idx-node        :node
         idx-constraints :constraints
         idx-env         :env}
        (gen/gen-node* (n/aset-idx current-node) target-env)
        {val-type        :type
         val-node        :node
         val-constraints :constraints
         val-env         :env}
        (gen/gen-node* (n/aset-val current-node) idx-env)
        elem-tv (cond
                  (ty/vec-type? target-type)
                  (ty/vec-element-type target-type)
                  (ty/hetero-vec? target-type)
                  (let [idx-val (when (n/literal-node? idx-node) (n/lit-val idx-node))]
                    (when idx-val (nth (ty/hetero-vec-types target-type) idx-val nil)))
                  :else (gen/fresh-tvar))
        len-tv  (cond
                  (ty/vec-type? target-type) (ty/vec-size target-type)
                  :else (gen/fresh-tvar))
        vec-tv    (ty/make-tvec elem-tv len-tv)
        target-eq (when (and (not (ty/vec-type? target-type))
                             (not (ty/hetero-vec? target-type)))
                    (cons/make-cequal target-type vec-tv))
        val-eq    (cons/make-cequal val-type elem-tv)
        new-node  (n/make-aset target-node idx-node val-node
                               (n/node-meta current-node))
        constrs   (concat target-constraints idx-constraints val-constraints
                          (when target-eq [target-eq])
                          [val-eq])]
    {:type        nil
     :node        (ty/set-type! new-node nil)
     :constraints constrs
     :env         val-env}))

;; ── alength ────────────────────────────────
(defmethod gen/gen-node* :alength [current-node context]
  (let [{target-type        :type
         target-node        :node
         target-constraints :constraints
         target-env         :env}
        (gen/gen-node* (n/alength-target current-node) context)
        int-ty (ty/make-tcon (tp/integer-type (u/frontend context)))]
    (if (and (ty/vec-type? target-type)
             (integer? (ty/type-value? (ty/vec-size target-type))))
      (let [len-val  (ty/value-val (ty/vec-size target-type))
            lit-node (n/make-literal len-val
                                     (ir2/attrs current-node)
                                     (ir2/node-meta current-node))]
        {:type        int-ty
         :node        (ty/set-type! lit-node int-ty)
         :constraints target-constraints
         :env         target-env})
      (let [new-node (n/make-alength target-node (n/node-meta current-node))]
        {:type        int-ty
         :node        (ty/set-type! new-node int-ty)
         :constraints target-constraints
         :env         target-env}))))
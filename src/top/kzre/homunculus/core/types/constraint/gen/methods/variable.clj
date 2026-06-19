(ns top.kzre.homunculus.core.types.constraint.gen.methods.variable
  "约束生成：变量节点。优先节点自身类型，其次环境，最后符号表。
   支持符号表重载：类型条目优先。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.utils :as u]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defmethod gen/cg-node-raw :variable [node context]
  (let [name (n/var-name node)
        ;; 1. 优先节点已有类型（用户标注或前序推断）
        existing (ty/get-type node (u/known-types context))]
    (if existing
      [existing (ty/set-type! node existing) nil context]
      (let [env (u/env context)
            ;; 2. 局部环境绑定
            binding (or (e/lookup-env env name)
                        (e/lookup-env env (symbol name)))]
        (if binding
          (let [ty (if (scheme/tscheme? binding)
                     (scheme/instantiate binding)
                     binding)]
            [ty (ty/set-type! node ty) nil context])
          ;; 3. 全局符号表（支持重载）
          (if-let [raw-entry (sym/lookup-in-tables name (u/symbol-table context))]
            ;; 优先使用类型条目（record/variable/primitive）
            (if-let [type-entry (sym/entry->type raw-entry)]
              (let [ty (:type type-entry)]
                (if ty
                  [ty (ty/set-type! node ty) nil context]
                  (let [tv (gen/fresh-tvar)]
                    [tv (ty/set-type! node tv) nil context])))
              ;; 否则分配新类型变量（函数条目不在此处确定类型）
              (let [tv (gen/fresh-tvar)]
                [tv (ty/set-type! node tv) nil context]))
            ;; 无任何信息，分配新类型变量
            (let [tv (gen/fresh-tvar)]
              [tv (ty/set-type! node tv) nil context])))))))
(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.variable
  "约束生成：变量节点。优先节点自身类型，其次环境，最后符号表。
   支持符号表重载：类型条目优先。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.pass.env :as e]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.symbol :as sym]))

(defn- fresh-result
  "为节点分配一个全新的类型变量，无约束，环境不变。"
  [current-node context]
  (let [tv (gen/fresh-tvar)]
    {:type        tv
     :node        (ty/set-type! current-node tv)
     :constraints nil
     :env         context}))

(defmethod gen/gen-node* :variable [current-node context]
  (let [name     (n/var-name current-node)
        ;; 1. 优先节点已有类型（用户标注或前序推断）
        existing (ty/get-type current-node)]
    (if existing
      {:type        existing
       :node        (ty/set-type! current-node existing)
       :constraints nil
       :env         context}
      (let [env     (u/env context)
            ;; 2. 局部环境绑定
            binding (or (e/lookup-env env name)
                        (e/lookup-env env (symbol name)))]
        (if binding
          (let [type (if (scheme/tscheme? binding)
                       (scheme/instantiate binding)
                       binding)]
            {:type        type
             :node        (ty/set-type! current-node type)
             :constraints nil
             :env         context})
          ;; 3. 全局符号表（支持重载）
          (if-let [raw-entry (sym/lookup-in-tables name (u/symbol-table context))]
            ;; 优先使用类型条目（record/variable/primitive）
            (if-let [type-entry (sym/entry->type raw-entry)]
              (if-let [type (:type type-entry)]
                {:type        type
                 :node        (ty/set-type! current-node type)
                 :constraints nil
                 :env         context}
                (fresh-result current-node context))
              ;; 函数条目不在此处确定类型
              (fresh-result current-node context))
            ;; 无任何信息
            (fresh-result current-node context)))))))
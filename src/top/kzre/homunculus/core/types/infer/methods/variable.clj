(ns top.kzre.homunculus.core.types.infer.methods.variable
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.infer.core :as core]
    [top.kzre.homunculus.core.types.type :as t]
    [top.kzre.homunculus.internal.symbol :as sym]))

(ns top.kzre.homunculus.core.types.infer.methods.variable
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.infer.core :as core]
    [top.kzre.homunculus.core.types.type :as t]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defmethod core/local-infer :variable [node context]
  (let [env  (core/env context)
        name (n/var-name node)
        ;; 1. 最高优先级：节点自身的类型（用户标注 + 前序推断结果）
        existing (t/get-type node (core/known-types context))]
    (if existing
      ;; 已有类型，直接使用，不改变环境
      (core/success existing node context)
      ;; 2. 次级：局部环境绑定（函数参数、let 引入等）
      (if-let [binding (e/lookup-env env name)]
        (core/success binding (t/ensure-type node binding) context)
        ;; 3. 最低回退：全局符号表（支持重载）
        (if-let [raw-entry (sym/lookup-in-tables name (core/symbol-table context))]
          ;; 3a. 如果条目包含函数，不在此确定类型，交给约束求解
          (if (sym/entry->func raw-entry)
            (core/nothing node context)
            ;; 3b. 否则尝试提取类型条目（记录、变量、原始类型）
            (if-let [type-entry (sym/entry->type raw-entry)]
              (let [ty (:type type-entry)]
                (if ty
                  (let [new-env (e/extend-env env name ty)]
                    (core/success ty (t/ensure-type node ty) (core/new-env context new-env)))
                  (core/nothing node context)))
              (core/nothing node context)))
          (core/nothing node context))))))
(ns top.kzre.homunculus.core.types.constraint.gen.methods.variable
  "约束生成：变量节点。优先查找当前环境，其次查询符号表（内置+用户），
   若都无则分配新 TVar。不再回退旧 API。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defmethod gen/cg-node-raw :variable [node context]
  (let [env     (:env context)
        name    (n/var-name node)
        binding (or (e/lookup-env env name)
                    (e/lookup-env env (symbol name)))]
    (if binding
      (let [ty (if (scheme/tscheme? binding)
                 (scheme/instantiate binding)
                 binding)]
        [ty (ty/set-type! node ty) nil])
      ;; 查找符号表
      (if-let [entry (sym/lookup-in-tables name (:symbol-table context))]
        (let [ty (cond
                   (sym/function-symbol? entry)
                   (when-let [first-arity (first (sym/list-arities entry))]
                     (some-> (:ret first-arity) :type))
                   (sym/record-symbol? entry) (:type entry)
                   (sym/variable-symbol? entry) (:type entry)
                   :else nil)]
          (if ty
            [ty (ty/set-type! node ty) nil]
            (let [tv (gen/fresh-tvar)]
              [tv (ty/set-type! node tv) nil])))
        (let [tv (gen/fresh-tvar)]
          [tv (ty/set-type! node tv) nil])))))
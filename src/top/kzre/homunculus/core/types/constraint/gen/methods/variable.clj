(ns top.kzre.homunculus.core.types.constraint.gen.methods.variable
  "约束生成：变量节点。优先查找当前环境，其次查询前端内置函数。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :variable [node context]
  (let [env (:env context)
        name (n/var-name node)            ;; 使用 ir2.node 工具
        binding (or (e/lookup-env env name)
                    (e/lookup-env env (symbol name)))]
    (if binding
      (let [ty (if (scheme/tscheme? binding)
                 (scheme/instantiate binding)
                 binding)]
        [ty (ty/set-type! node ty) nil])
      ;; 查找前端内置函数
      (if-let [frontend (:frontend context)]
        (if-let [builtin-ty (get (tp/builtin-functions frontend) name)]
          [builtin-ty (ty/set-type! node builtin-ty) nil]
          (let [tv (gen/fresh-tvar)]
            [tv (ty/set-type! node tv) nil]))
        (let [tv (gen/fresh-tvar)]
          [tv (ty/set-type! node tv) nil])))))
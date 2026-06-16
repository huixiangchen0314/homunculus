(ns top.kzre.homunculus.core.types.constraint.gen.methods.variable
  "约束生成：变量节点。优先查找当前环境，其次查询前端内置函数。
   如果两者都没有，则分配全新的类型变量。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.env :as e]
    [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :variable [node context]
  (let [env     (:env context)
        name    (n/var-name node)
        ;; 1. 在当前环境中查找绑定
        binding (or (e/lookup-env env name)
                    (e/lookup-env env (symbol name)))]
    (if binding
      ;; 环境中有绑定 → 实例化可能的 TScheme 后直接使用
      (let [ty (if (scheme/tscheme? binding)
                 (scheme/instantiate binding)
                 binding)]
        [ty (ty/set-type! node ty) nil])
      ;; 2. 环境无绑定，尝试从前端内置函数表查找
      (if-let [frontend (:frontend context)]
        (if-let [builtin-ty (get (tp/builtin-functions frontend) name)]
          [builtin-ty (ty/set-type! node builtin-ty) nil]
          ;; 3. 既无绑定也无内置函数 → 分配全新类型变量
          (let [tv (gen/fresh-tvar)]
            [tv (ty/set-type! node tv) nil]))
        ;; 无前端实例，直接分配新变量
        (let [tv (gen/fresh-tvar)]
          [tv (ty/set-type! node tv) nil])))))
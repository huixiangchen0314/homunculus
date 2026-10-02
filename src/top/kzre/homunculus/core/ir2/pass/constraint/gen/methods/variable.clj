(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.variable
  "约束生成：变量节点。
   优先节点自身类型；否则由 env 统一解析（本地绑定优先，全局符号表兜底）。"
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod gen/gen-node* :variable [current-node env]
  (if-let [type (or (ty/get-type current-node)
                    (p/resolve-var-type env (n/var-name current-node)))]
    {:type        type
     :node        (ty/set-type! current-node type)
     :constraints nil
     :env         env}
    (let [tv (gen/fresh-tvar)]
      {:type        tv
       :node        (ty/set-type! current-node tv)
       :constraints nil
       :env         env})))
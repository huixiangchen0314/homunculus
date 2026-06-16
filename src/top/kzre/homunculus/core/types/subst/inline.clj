(ns top.kzre.homunculus.core.types.subst.inline
  "Lambda 内联工具：将调用点的形参替换为实参，展开函数体。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.subst.replace :as r]))

(defmulti inline-expr
          (fn [node _subst-map] (n/kind node)))

(defmethod inline-expr :default [node subst-map]
  ;; 通用递归：不直接处理，而是留给 inline-call 专门使用
  (throw (ex-info "inline-expr should not be called directly; use inline-call" {})))

(defn inline-call [call-node lambda-node _config]
  "将 lambda 形参替换为 call 实参，返回展开后的 body。"
  (let [params (n/lambda-params lambda-node)
        args   (n/call-args call-node)
        body   (n/lambda-body lambda-node)]
    (reduce (fn [body [param arg]]
              (r/replace-var body (n/var-name param) arg))
            body
            (map vector params args))))
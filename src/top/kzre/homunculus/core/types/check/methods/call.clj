(ns top.kzre.homunculus.core.types.check.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :call [node expected context]
  (let [fn-node     (check/check-node (n/call-fn node) nil context)
        fn-name     (when (= (n/kind fn-node) :variable)
                      (n/var-name fn-node))
        ;; 从前端协议查找内置函数类型
        builtin-ty  (when fn-name
                      (get (tp/builtin-functions (:frontend context)) fn-name))
        ;; 优先使用内置类型，其次节点已有类型
        fn-ty       (or builtin-ty (ty/get-type fn-node))
        ;; 若函数类型是多态 Scheme，实例化
        fn-ty       (if (ty/scheme-type? fn-ty)
                      (scheme/instantiate fn-ty)
                      fn-ty)
        args        (n/call-args node)]
    (if (and fn-ty (ty/fun-type? fn-ty))
      (let [arg-types    (take-while some? (map ty/fun-arg (take (count args) (iterate ty/fun-ret fn-ty))))
            checked-args (mapv (fn [arg ty] (check/check-node arg (when (some? ty) ty) context)) args arg-types)
            ret-node     (n/make-call fn-node checked-args
                                      (n/attrs node) (n/node-meta node) (n/parent node))]
        (if expected
          (check/check-type ret-node expected context)
          ret-node))
      node)))
(ns top.kzre.homunculus.core.types.check.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :call [node expected context]
  (let [fn-node (check/check-node (n/call-fn node) nil context)
        ;; 直接使用节点已有的类型（约束求解后已确定）
        fn-ty    (ty/get-type fn-node)
        ;; 若为 Scheme 则实例化
        fn-ty    (if (ty/scheme-type? fn-ty)
                   (scheme/instantiate fn-ty)
                   fn-ty)
        args     (n/call-args node)]
    (if (and fn-ty (ty/fun-type? fn-ty))
      (let [arg-types    (take-while some? (map ty/fun-arg (take (count args) (iterate ty/fun-ret fn-ty))))
            checked-args (mapv (fn [arg ty] (check/check-node arg (when (some? ty) ty) context)) args arg-types)
            ret-node     (n/make-call fn-node checked-args
                                      (n/attrs node) (n/node-meta node) (n/parent node))]
        (if expected
          (check/check-type ret-node expected context)
          ret-node))
      ;; 若无函数类型，直接返回原节点（可能推导失败，由其他 Pass 报错）
      node)))
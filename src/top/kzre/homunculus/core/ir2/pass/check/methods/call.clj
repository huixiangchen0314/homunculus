(ns top.kzre.homunculus.core.ir2.pass.check.methods.call
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :call [node expected env]
  (let [fn-node (check/check-node* (n/call-fn node) nil env)
        ;; 直接使用节点已有的类型（约束求解后已确定）
        fn-type (ty/get-type fn-node)
        ;; 若为 Scheme 则实例化
        fn-type (if (ty/scheme-type? fn-type)
                  (scheme/instantiate fn-type)
                  fn-type)
        args    (n/call-args node)]
    (if (and fn-type (ty/fun-type? fn-type))
      (let [arg-types    (take-while some? (map ty/fun-param
                                                (take (count args)
                                                      (iterate ty/fun-ret fn-type))))
            checked-args (mapv (fn [arg type]
                                 (check/check-node* arg (when (some? type) type) env))
                               args arg-types)
            ret-node     (n/make-call fn-node checked-args
                                      (n/attrs node)
                                      (n/node-meta node))]
        (if expected
          (check/check-type ret-node expected env)
          ret-node))
      node)))
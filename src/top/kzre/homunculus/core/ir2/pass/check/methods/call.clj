(ns top.kzre.homunculus.core.ir2.pass.check.methods.call
  (:require
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod check/check-node* :call [node expected env]
  (let [fn-node (check/check-node* (n/call-fn node) nil env)
        fn-type (ty/get-type fn-node)
        fn-type (if (ty/scheme-type? fn-type)
                  (scheme/instantiate fn-type)
                  fn-type)
        args    (n/call-args node)]
    ;; 1. callee 必须是函数类型
    (when-not (ty/fun-type? fn-type)
      (throw (err/compile-error
               :not-a-function
               "call target is not a function type"
               {:node node :fn-type fn-type})))
    ;; 2. 展开柯里化参数列表，长度 = 实参数量
    (let [arg-types (mapv ty/fun-param
                          (take (count args) (iterate ty/fun-ret fn-type)))]
      ;; 3. 柯里化链必须在实参耗尽前不中断
      ;;    arg-types 中出现 nil → 实参数量超过函数接受的参数数量
      (when (some nil? arg-types)
        (throw (err/compile-error
                 :arity-mismatch
                 "call has more arguments than the function accepts"
                 {:node      node
                  :arg-count (count args)
                  :fn-type   fn-type})))
      ;; 4. 逐实参检查，期望类型为对应形参类型
      (let [checked-args (mapv (fn [arg arg-type]
                                 (check/check-node* arg arg-type env))
                               args arg-types)
            ret-node     (n/make-call fn-node checked-args
                                      (n/attrs node)
                                      (n/node-meta node))]
        ;; 5. 返回值验证
        (check/check-type ret-node expected env)))))
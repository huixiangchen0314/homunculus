(ns top.kzre.homunculus.core.ir2.pass.check.methods.call
  (:require
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))



(defmethod check/check-node* :call [node expected env]
  (let [fn-node (check/check-node* (n/call-fn node) nil env)
        ;fn-name (n/var-name (n/call-fn node)) ;; for debug
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
    (let [{:keys [params return]} (ty/fun-params-return fn-type)
          arg-count        (count args)]
      ;; 3. arity 检查：严格的参数相当，柯里化，部分应用走专用节点
      (when (not= arg-count (count params))
        (throw (err/compile-error
                 :arity-mismatch
                 "call arity must exactly match the function's parameter count; use apply/partial for currying"
                 {:node      node
                  :arg-count arg-count
                  :fn-type   fn-type})))
      ;; 4. 取前 arg-count 个形参类型，逐实参检查
      (let [arg-types    (subvec params 0 arg-count)
            checked-args (mapv (fn [arg arg-type]
                                 (check/check-node* arg arg-type env))
                               args arg-types)
            ret-node     (n/make-call fn-node checked-args
                                      (n/attrs node)
                                      (n/node-meta node))
            ret-node' (ty/set-type! ret-node return)]
        ;; 5. 返回值验证
        (check/check-type ret-node' expected env)))))
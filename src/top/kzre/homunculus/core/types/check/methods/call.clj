(ns top.kzre.homunculus.core.types.check.methods.call
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defmethod check/check :call [node expected context]
  (let [fn-node (check/check (:fn node) nil context)
        fn-ty (get-in fn-node [:attrs :type])
        ;; 从函数类型中提取参数类型列表（假设柯里化）
        arg-types (if (instance? TFun fn-ty)
                    (take-while #(instance? TFun %) (iterate :ret fn-ty))
                    [])
        args (:args node)
        ;; 对每个参数用对应的类型检查
        checked-args (mapv (fn [arg expected-ty]
                             (check/check arg expected-ty context))
                           args (take (count args) arg-types))
        ret-node (assoc node :fn fn-node :args checked-args)]
    (if expected
      (check/check-type ret-node expected context)
      ret-node)))
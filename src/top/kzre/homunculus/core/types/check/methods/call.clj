(ns top.kzre.homunculus.core.types.check.methods.call
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))


(defmethod check/check :call [node expected context]
  (let [fn-node (check/check (:fn node) nil context)
        ;; 优先使用 builtin-fn 类型
        builtin-ty (get-in fn-node [:attrs :builtin-fn])
        fn-ty (or builtin-ty (type/get-type fn-node))
        args (:args node)]
    (if (and fn-ty (instance? TFun fn-ty))
      (let [arg-types (take-while some? (map :arg (take (count args) (iterate :ret fn-ty))))
            checked-args (mapv (fn [arg ty] (check/check arg (when (some? ty) ty) context)) args arg-types)
            ret-node (assoc node :fn fn-node :args checked-args)]
        (if expected
          (check/check-type ret-node expected context)
          ret-node))
      ;; 无法获取函数类型，直接原样返回（跳过参数检查）
      node)))
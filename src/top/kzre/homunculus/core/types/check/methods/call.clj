(ns top.kzre.homunculus.core.types.check.methods.call
  (:require [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

;; top.kzre.homunculus.core.types.check.methods.call

(defmethod check/check :call [node expected context]
  (let [fn-node (check/check (:fn node) nil context)
        fn-ty   (get-in fn-node [:attrs :type])
        ;; 正确提取柯里化参数类型
        arg-types (if (instance? TFun fn-ty)
                    (take-while some? (map :arg (take (count (:args node))
                                                      (iterate :ret fn-ty))))
                    [])
        args     (:args node)
        checked-args (mapv (fn [arg ty]
                             (check/check arg (when (some? ty) ty) context))
                           args arg-types)
        ret-node (assoc node :fn fn-node :args checked-args)]
    (if expected
      (check/check-type ret-node expected context)
      ret-node)))
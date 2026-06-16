(ns top.kzre.homunculus.core.types.check.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.type :as type])
  (:import (top.kzre.homunculus.core.types.model TFun)))

(defmethod check/check-node :call [node expected context]
  (let [fn-node (check/check-node (n/call-fn node) nil context)
        builtin-ty (get-in fn-node [:attrs :builtin-fn])
        fn-ty (or builtin-ty (type/get-type fn-node))
        ;; 若函数类型是多态 Scheme，实例化
        fn-ty (if (scheme/tscheme? fn-ty)
                (scheme/instantiate fn-ty)
                fn-ty)
        args (n/call-args node)]
    (if (and fn-ty (instance? TFun fn-ty))
      (let [arg-types (take-while some? (map :arg (take (count args) (iterate :ret fn-ty))))
            checked-args (mapv (fn [arg ty] (check/check-node arg (when (some? ty) ty) context)) args arg-types)
            ret-node (n/call-with-children node fn-node checked-args)]
        (if expected
          (check/check-type ret-node expected context)
          ret-node))
      node)))
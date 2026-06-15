(ns top.kzre.homunculus.core.types.check.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod check/check :loop [node expected context]
  (let [bindings (n/loop-bindings node)
        checked-bindings (mapv (fn [[var val]]
                                 [(check/check var nil context)
                                  (check/check val nil context)])
                               bindings)
        loop-var-tys (mapv (fn [[var _]] (type/get-type var)) checked-bindings)
        body-context (assoc context :loop-var-tys loop-var-tys)
        body-node (check/check (n/loop-body node) nil body-context)]
    (n/loop-with-children node checked-bindings body-node)))

(defmethod check/check :recur [node expected context]
  (let [loop-var-tys (get context :loop-var-tys)]
    (when-not loop-var-tys
      (throw (ex-info "recur outside loop" {})))
    (let [args (n/recur-args node)
          _ (when (not= (count args) (count loop-var-tys))
              (throw (ex-info "recur arg count mismatch" {})))
          checked-args (mapv (fn [arg exp-ty] (check/check arg exp-ty context))
                             args loop-var-tys)]
      (assoc node :args checked-args))))
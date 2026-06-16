(ns top.kzre.homunculus.core.types.check.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :loop [node expected context]
  (let [bindings (n/loop-bindings node)
        checked-bindings (mapv (fn [[var val]]
                                 (n/make-binding (check/check-node var nil context)
                                                 (check/check-node val nil context)))
                               bindings)
        loop-var-tys (mapv (fn [[var _]] (ty/get-type var)) checked-bindings)
        body-context (assoc context :loop-var-tys loop-var-tys)
        body-node (check/check-node (n/loop-body node) nil body-context)]
    (n/make-loop checked-bindings body-node
                 (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod check/check-node :recur [node expected context]
  (let [loop-var-tys (get context :loop-var-tys)]
    (when-not loop-var-tys
      (throw (ex-info "recur outside loop" {})))
    (let [args (n/recur-args node)
          _ (when (not= (count args) (count loop-var-tys))
              (throw (ex-info "recur arg count mismatch" {})))
          checked-args (mapv (fn [arg exp-ty] (check/check-node arg exp-ty context))
                             args loop-var-tys)]
      (n/make-recur checked-args (n/attrs node) (n/node-meta node) (n/parent node)))))
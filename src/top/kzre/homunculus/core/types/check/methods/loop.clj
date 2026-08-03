(ns top.kzre.homunculus.core.types.check.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod check/check-node :loop [node expected context]
  (let [bindings (n/loop-bindings node)          ;; Binding 向量
        checked-bindings (mapv (fn [b]
                                 (let [new-var (check/check-node (:var b) nil context)
                                       new-val (check/check-node (:val b) nil context)]
                                   (assoc b :var new-var :val new-val)))
                               bindings)
        loop-var-tys (mapv (fn [b] (ty/get-type (:var b))) checked-bindings)
        body-context (assoc context :loop-var-tys loop-var-tys)
        body-node (check/check-node (n/loop-body node) nil body-context)]
    (n/make-loop checked-bindings body-node
                 (n/attrs node) (n/node-meta node))))

(defmethod check/check-node :recur [node expected context]
  (let [loop-var-tys (get context :loop-var-tys)]
    (when-not loop-var-tys
      (throw (ex-info "recur outside loop" {})))
    (let [args (:args node)
          _ (when (not= (count args) (count loop-var-tys))
              (throw (ex-info "recur arg count mismatch" {})))
          checked-args (mapv (fn [arg exp-ty] (check/check-node arg exp-ty context))
                             args loop-var-tys)]
      (n/make-recur checked-args (n/attrs node) (n/node-meta node)))))
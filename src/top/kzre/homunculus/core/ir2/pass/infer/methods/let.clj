(ns top.kzre.homunculus.core.ir2.pass.infer.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod infer/infer-node* :let [node env]
  (let [bindings (n/let-bindings node)        ;; Binding 向量
        [bind-nodes final-env]
        (reduce (fn [[bnds current-env] b]
                  (let [var-node (:var b)
                        val-node (:val b)
                        [val-type val-new val-env] (infer/infer-node* val-node current-env)
                        var-name    (:name var-node)
                        next-env    (if val-type
                                      (p/bind-var val-env var-name val-type)
                                      val-env)
                        var-new     (if val-type
                                      (t/ensure-type var-node val-type)
                                      var-node)
                        new-binding (assoc b :var var-new :val val-new)]
                    [(conj bnds new-binding) next-env]))
                [[] env]
                bindings)
        [body-type body-node body-env] (infer/infer-node* (n/let-body node) final-env)]
    (if body-type
      (let [new-node   (n/make-let (vec bind-nodes) body-node
                                   (n/attrs node) (n/node-meta node))
            typed-node (t/set-type! new-node body-type)]
        (infer/success body-type typed-node body-env))
      (let [new-node (n/make-let (vec bind-nodes) body-node
                                 (n/attrs node) (n/node-meta node))]
        (infer/nothing new-node body-env)))))
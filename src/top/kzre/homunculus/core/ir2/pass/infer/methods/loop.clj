(ns top.kzre.homunculus.core.ir2.pass.infer.methods.loop
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as type]))

(defmethod infer/infer-node* :loop [node env]
  (let [bindings (n/loop-bindings node)          ;; Binding 向量
        [bind-nodes final-env]
        (reduce (fn [[bnds current-env] b]       ;; b 是 Binding 记录
                  (let [var-node (:var b)
                        val-node (:val b)
                        [val-type val-new val-env] (infer/infer-node* val-node current-env)
                        var-name    (:name var-node)
                        next-env    (if val-type
                                      (p/bind-var val-env var-name val-type)
                                      val-env)
                        var-new     (if val-type
                                      (type/set-type! var-node val-type)
                                      var-node)
                        new-b       (assoc b :var var-new :val val-new)]
                    [(conj bnds new-b) next-env]))
                [[] env]
                bindings)
        [body-type body-node body-env] (infer/infer-node* (n/loop-body node) final-env)]
    (if body-type
      (let [new-node   (n/make-loop (vec bind-nodes) body-node
                                    (n/attrs node) (n/node-meta node))
            typed-node (type/set-type! new-node body-type)]
        (infer/success body-type typed-node body-env))
      (let [new-node (n/make-loop (vec bind-nodes) body-node
                                  (n/attrs node) (n/node-meta node))]
        (infer/nothing new-node body-env)))))
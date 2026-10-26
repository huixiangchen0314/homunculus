(ns top.kzre.homunculus.core.ir2.pass.infer.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod infer/infer-node* :let [node env]
  (let [bindings (n/let-bindings node)        ;; Binding 向量
        [bind-nodes final-env] (infer/infer-seq bindings env)
        [body-type body-node body-env] (infer/infer-node* (n/let-body node) final-env)]
    (if body-type
      (let [new-node   (n/make-let (vec bind-nodes) body-node
                                   (n/attrs node) (n/node-meta node))
            typed-node (t/set-type! new-node body-type)]
        (infer/success body-type typed-node body-env))
      (let [new-node (n/make-let (vec bind-nodes) body-node
                                 (n/attrs node) (n/node-meta node))]
        (infer/nothing new-node body-env)))))
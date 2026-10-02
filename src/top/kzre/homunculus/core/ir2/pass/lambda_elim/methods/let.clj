(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.core :as elim]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.env :as p]))

(defmethod elim/elim-node* :let [node env]
  (let [bindings (n/let-bindings node)         ;; Binding 向量
        ;; 值表达式在外部环境下处理
        [new-bindings val-defs]
        (reduce (fn [[bnds defs] b]
                  (let [val-node (:val b)
                        [new-val val-defs'] (elim/elim-node* val-node env)
                        new-b (assoc b :val new-val)]
                    [(conj bnds new-b) (into defs val-defs')]))
                [[] []]
                bindings)
        ;; 扩展内部环境——绑定名可见于 body
        binding-names (map #(:name (:var %)) bindings)
        inner-env     (reduce p/bind-var env binding-names)
        [new-body body-defs] (elim/elim-node* (n/let-body node) inner-env)
        new-let (n/make-let new-bindings new-body
                            (n/attrs node) (n/node-meta node))]
    [new-let (into val-defs body-defs)]))
(ns top.kzre.homunculus.core.ir2.pass.lambda-elim.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.core :as elim]
            [top.kzre.homunculus.core.ir2.pass.lambda-elim.env :as p]))

(defmethod elim/elim-node* :loop [node env]
  (let [bindings (n/loop-bindings node)        ;; Binding 向量
        ;; 值表达式在外部环境下处理
        [new-vals val-defs]
        (reduce (fn [[vals defs] b]
                  (let [val-node (:val b)
                        [new-val val-defs'] (elim/elim-node* val-node env)]
                    [(conj vals new-val) (into defs val-defs')]))
                [[] []]
                bindings)
        ;; 扩展内部环境——绑定名可见于 body
        binding-names (map #(:name (:var %)) bindings)
        inner-env     (reduce p/bind-var env binding-names)
        [new-body body-defs] (elim/elim-node* (n/loop-body node) inner-env)
        new-bindings (mapv (fn [b new-val] (assoc b :val new-val))
                           bindings new-vals)]
    [(n/make-loop new-bindings new-body
                  (n/attrs node) (n/node-meta node))
     (into val-defs body-defs)]))
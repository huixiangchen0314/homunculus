(ns top.kzre.homunculus.core.types.lambda-elim.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :loop [node config env]
  (let [bindings (n/loop-bindings node)         ;; 现在是 Binding 向量
        ;; 处理所有值表达式，使用外部环境；变量节点保持不变
        [new-vals val-defs]
        (reduce (fn [[vals defs] b]             ;; b 是 Binding 记录
                  (let [val-node (:val b)
                        [new-val val-defs'] (elim/eliminate val-node config env)]
                    [(conj vals new-val) (into defs val-defs')]))
                [[] []]
                bindings)
        ;; 收集绑定变量名，扩展内部环境
        binding-names (map #(:name (:var %)) bindings)
        inner-env (into env binding-names)
        ;; 在扩展环境中处理 body
        [new-body body-defs] (elim/eliminate (n/loop-body node) config inner-env)
        ;; 重建绑定：用消除后的值表达式更新每个 binding
        new-bindings (mapv (fn [b new-val] (assoc b :val new-val))
                           bindings new-vals)]
    [(n/make-loop new-bindings new-body
                  (n/attrs node) (n/node-meta node))
     (into val-defs body-defs)]))
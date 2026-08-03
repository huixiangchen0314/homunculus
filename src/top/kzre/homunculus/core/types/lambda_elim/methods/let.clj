(ns top.kzre.homunculus.core.types.lambda-elim.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :let [node config env]
  (let [bindings (n/let-bindings node)         ;; 现在是 Binding 向量
        ;; 处理所有值表达式，使用外部环境；变量节点保留
        [new-bindings val-defs]
        (reduce (fn [[bnds defs] b]
                  (let [val-node (:val b)
                        [new-val val-defs'] (elim/eliminate val-node config env)
                        new-b (assoc b :val new-val)]
                    [(conj bnds new-b) (into defs val-defs')]))
                [[] []]
                bindings)
        ;; 收集绑定变量名，扩展内部环境
        binding-names (map #(:name (:var %)) bindings)
        inner-env (into env binding-names)
        ;; 在扩展环境中处理 body
        [new-body body-defs] (elim/eliminate (n/let-body node) config inner-env)
        ;; 构造新 let 节点
        new-let (n/make-let new-bindings new-body
                            (n/attrs node) (n/node-meta node))]
    [new-let (into val-defs body-defs)]))
(ns top.kzre.homunculus.core.types.lambda-elim.methods.loop
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :loop [node config env]
  (let [bindings (n/loop-bindings node)
        ;; 处理所有右侧表达式，使用外部环境
        [new-vals val-defs]
        (reduce (fn [[vals defs] [_ e]]
                  (let [[new-e e-defs] (elim/eliminate e config env)]
                    [(conj vals new-e) (into defs e-defs)]))
                [[] []]
                bindings)
        ;; 收集绑定变量名，扩展内部环境
        binding-names (map (fn [[v]] (n/var-name v)) bindings)
        inner-env (into env binding-names)
        ;; 使用扩展环境处理 body
        [new-body body-defs] (elim/eliminate (n/loop-body node) config inner-env)
        ;; 变量节点本身不消除
        new-vars (mapv first bindings)]
    [(n/make-loop (mapv vector new-vars new-vals) new-body
                  (n/attrs node) (n/node-meta node) (n/parent node))
     (into val-defs body-defs)]))
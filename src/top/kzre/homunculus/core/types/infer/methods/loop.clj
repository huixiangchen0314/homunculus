(ns top.kzre.homunculus.core.types.infer.methods.loop
  (:require [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :loop [node context]
  (let [bindings (n/loop-bindings node)          ;; 现在返回 Binding 向量
        [bind-nodes final-ctx]
        (reduce (fn [[bnds ctx] b]               ;; b 是 Binding 记录
                  (let [var-node (:var b)
                        val-node (:val b)
                        [val-ty val-new val-ctx] (infer/local-infer val-node ctx)
                        var-name (:name var-node)
                        cur-env  (infer/env val-ctx)
                        new-env  (if val-ty
                                   (e/extend-env cur-env var-name val-ty)
                                   cur-env)
                        var-new  (if val-ty
                                   (type/set-type! var-node val-ty)
                                   var-node)
                        next-ctx (if val-ty
                                   (infer/new-env val-ctx new-env)
                                   val-ctx)
                        new-b    (assoc b :var var-new :val val-new)]
                    [(conj bnds new-b) next-ctx]))
                [[] context]
                bindings)
        [body-ty body-node body-ctx] (infer/local-infer (n/loop-body node) final-ctx)]
    (if body-ty
      (let [new-node   (n/make-loop (vec bind-nodes) body-node
                                    (n/attrs node) (n/node-meta node))
            typed-node (type/set-type! new-node body-ty)]
        (infer/success body-ty typed-node body-ctx))
      (let [new-node (n/make-loop (vec bind-nodes) body-node
                                  (n/attrs node) (n/node-meta node))]
        (infer/nothing new-node body-ctx)))))
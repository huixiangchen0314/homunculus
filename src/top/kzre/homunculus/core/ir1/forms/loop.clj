(ns top.kzre.homunculus.core.ir1.forms.loop
  "loop* 和 recur 的 IR1 构建。所有字段访问通过 ir1.node 工具函数。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

;; ── loop* ─────────────────────────────────
(defmethod ir1/form->node 'loop* [form]
  (let [[_ bindings & body] form]
    (n/make-loop bindings body (meta form))))

(defmethod ir1/build-tree :loop [node]
  (let [bindings   (n/loop-bindings node)
        body       (n/loop-body node)
        meta       (n/node-meta node)
        parent     (n/parent node)
        bind-pairs (n/binding-pairs bindings)
        ir-bindings (mapcat (fn [pair]
                              (let [[sym val] pair]
                                (n/make-binding (ir1/->ir1 sym) (ir1/->ir1 val))))
                            bind-pairs)
        ir-body-exprs (mapv ir1/->ir1 body)
        wrapped-body (n/wrap-body ir-body-exprs)]
    (n/make-loop ir-bindings wrapped-body meta parent)))

;; ── recur ─────────────────────────────────
(defmethod ir1/form->node 'recur [form]
  (let [[_ & exprs] form]
    (n/make-recur (vec exprs) (meta form))))

(defmethod ir1/build-tree :recur [node]
  (n/make-recur (mapv ir1/->ir1 (n/recur-exprs node))
                (n/node-meta node)
                (n/parent node)))
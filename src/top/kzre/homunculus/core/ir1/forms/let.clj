(ns top.kzre.homunculus.core.ir1.forms.let
  "let* 特殊形式的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'let [form]
  (let [[_ bindings & body] form]
    (n/make-let bindings body (meta form))))   ;; body 仍为原始表单向量

(defmethod ir1/build-tree :let [node]
  (let [bindings   (n/let-bindings node)
        body       (n/let-body node)            ;; 原始表单向量
        meta       (n/node-meta node)
        parent     (n/parent node)
        bind-pairs (n/binding-pairs bindings)
        ir-bindings (mapcat (fn [[sym val]]
                              (n/make-binding (ir1/->ir1 sym) (ir1/->ir1 val)))
                            bind-pairs)
        ir-body-exprs (mapv ir1/->ir1 body)    ;; 将 body 的每个表单转为 IR1 节点
        wrapped-body (n/wrap-body ir-body-exprs)] ;; 合并为单个节点（DoNode 或单节点）
    (n/make-let ir-bindings wrapped-body meta parent)))
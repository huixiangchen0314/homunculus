(ns top.kzre.homunculus.core.types.check.methods.try
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :try [node expected context]
  ;; body 是单个节点（可能为 BlockNode），直接递归检查
  (let [new-body    (check/check-node (n/try-body node) expected context)
        ;; catches 是 CatchNode 列表，每个 catch 会通过自己的 defmethod 递归处理
        new-catches (mapv #(check/check-node % expected context) (n/try-catches node))
        ;; finally 是单个节点或 nil
        new-finally (when-let [f (n/try-finally node)]
                      (check/check-node f nil context))]
    (n/make-try new-body new-catches new-finally
                (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod check/check-node :catch [node expected context]
  (let [new-class (check/check-node (n/catch-class node) nil context)
        new-sym   (check/check-node (n/catch-sym node) nil context)
        new-body  (mapv #(check/check-node % expected context) (n/catch-body node))]
    (n/make-catch new-class new-sym new-body
                  (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod check/check-node :throw [node expected context]
  ;; throw 内的表达式仍要检查，但本身无期望类型
  (let [new-expr (check/check-node (n/throw-expr node) nil context)]
    (n/make-throw new-expr (n/attrs node) (n/node-meta node) (n/parent node))))
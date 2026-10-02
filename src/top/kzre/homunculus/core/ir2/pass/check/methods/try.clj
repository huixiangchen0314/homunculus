(ns top.kzre.homunculus.core.ir2.pass.check.methods.try
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node* :try [node expected env]
  ;; body 是单个节点（可能为 BlockNode），直接递归检查
  (let [new-body    (check/check-node* (n/try-body node) expected env)
        ;; catches 是 CatchNode 列表，每个 catch 会通过自己的 defmethod 递归处理
        new-catches (mapv #(check/check-node* % expected env) (n/try-catches node))
        ;; finally 是单个节点或 nil
        new-finally (when-let [f (n/try-finally node)]
                      (check/check-node* f nil env))]
    (n/make-try new-body new-catches new-finally
                (n/attrs node)
                (n/node-meta node))))

(defmethod check/check-node* :catch [node expected env]
  (let [new-class (check/check-node* (n/catch-class node) nil env)
        new-sym   (check/check-node* (n/catch-sym node) nil env)
        new-body  (mapv #(check/check-node* % expected env) (n/catch-body node))]
    (n/make-catch new-class new-sym new-body
                  (n/attrs node)
                  (n/node-meta node))))

(defmethod check/check-node* :throw [node expected env]
  ;; throw 内的表达式仍要检查，但本身无期望类型
  (let [new-expr (check/check-node* (n/throw-expr node) nil env)]
    (n/make-throw new-expr
                  (n/attrs node)
                  (n/node-meta node))))
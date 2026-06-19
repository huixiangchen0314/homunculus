(ns top.kzre.homunculus.core.types.infer.methods.try
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]))

(defmethod infer/local-infer :try [node context]
  ;; 推导 body（单个节点，通常是 block）
  (let [[_ new-body body-ctx] (infer/local-infer (n/try-body node) context)
        ;; 顺序推导各个 catch，累积上下文
        [new-catches catch-ctx]
        (reduce (fn [[catches ctx] c]
                  (let [[_ new-c c-ctx] (infer/local-infer c ctx)]
                    [(conj catches new-c) c-ctx]))
                [[] body-ctx]
                (n/try-catches node))
        ;; 推导 finally（如果有）
        [_ new-finally final-ctx] (if-let [f (n/try-finally node)]
                                    (infer/local-infer f catch-ctx)
                                    [nil nil catch-ctx])
        new-node (n/make-try new-body
                             new-catches
                             new-finally
                             (n/attrs node) (n/node-meta node) (n/parent node))]
    (infer/nothing new-node final-ctx)))

(defmethod infer/local-infer :catch [node context]
  ;; 顺序推导：异常类 -> 符号 -> body 序列
  (let [[_ new-class ctx1] (infer/local-infer (n/catch-class node) context)
        [_ new-sym ctx2]   (infer/local-infer (n/catch-sym node) ctx1)
        ;; body 是向量，逐表达式推导并累积上下文
        [new-body body-ctx]
        (reduce (fn [[exprs ctx] expr]
                  (let [[_ new-expr new-ctx] (infer/local-infer expr ctx)]
                    [(conj exprs new-expr) new-ctx]))
                [[] ctx2]
                (n/catch-body node))
        new-node (n/make-catch new-class
                               new-sym
                               new-body
                               (n/attrs node) (n/node-meta node) (n/parent node))]
    (infer/nothing new-node body-ctx)))

(defmethod infer/local-infer :throw [node context]
  (let [[_ new-expr expr-ctx] (infer/local-infer (n/throw-expr node) context)
        new-node (n/make-throw new-expr
                               (n/attrs node) (n/node-meta node) (n/parent node))]
    (infer/nothing new-node expr-ctx)))
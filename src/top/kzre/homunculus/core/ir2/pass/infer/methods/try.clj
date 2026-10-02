(ns top.kzre.homunculus.core.ir2.pass.infer.methods.try
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

(defmethod infer/infer-node* :try [node env]
  ;; 推导 body（单个节点，通常是 block）
  (let [[_ new-body body-env] (infer/infer-node* (n/try-body node) env)
        ;; 顺序推导各个 catch，累积环境
        [new-catches catch-env]
        (reduce (fn [[catches current-env] c]
                  (let [[_ new-c new-env] (infer/infer-node* c current-env)]
                    [(conj catches new-c) new-env]))
                [[] body-env]
                (n/try-catches node))
        ;; 推导 finally（如果有）
        [_ new-finally final-env] (if-let [f (n/try-finally node)]
                                    (infer/infer-node* f catch-env)
                                    [nil nil catch-env])
        new-node (n/make-try new-body
                             new-catches
                             new-finally
                             (n/attrs node)
                             (n/node-meta node))]
    (infer/nothing new-node final-env)))

(defmethod infer/infer-node* :catch [node env]
  ;; 顺序推导：异常类 → 符号 → body 序列
  (let [[_ new-class class-env] (infer/infer-node* (n/catch-class node) env)
        [_ new-sym sym-env]     (infer/infer-node* (n/catch-sym node) class-env)
        ;; body 是向量，逐表达式推导并累积环境
        [new-body body-env]
        (reduce (fn [[exprs current-env] expr]
                  (let [[_ new-expr new-env] (infer/infer-node* expr current-env)]
                    [(conj exprs new-expr) new-env]))
                [[] sym-env]
                (n/catch-body node))
        new-node (n/make-catch new-class
                               new-sym
                               new-body
                               (n/attrs node)
                               (n/node-meta node))]
    (infer/nothing new-node body-env)))

(defmethod infer/infer-node* :throw [node env]
  (let [[_ new-expr expr-env] (infer/infer-node* (n/throw-expr node) env)
        new-node (n/make-throw new-expr
                               (n/attrs node)
                               (n/node-meta node))]
    (infer/nothing new-node expr-env)))
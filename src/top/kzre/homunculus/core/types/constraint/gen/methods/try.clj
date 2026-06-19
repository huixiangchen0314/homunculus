(ns top.kzre.homunculus.core.types.constraint.gen.methods.try
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :try [node context]
  ;; 1. 处理 body
  (let [[body-tv body-node body-constr body-ctx] (gen/cg-node-raw (n/try-body node) context)
        ;; 2. 顺序处理 catches，累积上下文
        [catch-nodes catch-constr catch-ctx]
        (reduce (fn [[nodes constrs ctx] c]
                  (let [[_tv new-c cc new-ctx] (gen/cg-node-raw c ctx)]
                    [(conj nodes new-c) (into constrs cc) new-ctx]))
                [[] [] body-ctx]
                (n/try-catches node))
        ;; 3. 处理 finally（如果有）
        [finally-node finally-constr final-ctx]
        (if-let [f (n/try-finally node)]
          (let [[_tv new-f fc new-ctx] (gen/cg-node-raw f catch-ctx)]
            [new-f fc new-ctx])
          [nil nil catch-ctx])
        ;; try 整体类型为 body 类型
        tv (or body-tv (gen/fresh-tvar))
        new-node (n/make-try body-node
                             (vec catch-nodes)
                             finally-node
                             (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv)
     (concat body-constr catch-constr finally-constr)
     final-ctx]))

(defmethod gen/cg-node-raw :catch [node context]
  (let [[_class-tv class-node class-constr class-ctx] (gen/cg-node-raw (n/catch-class node) context)
        [_sym-tv sym-node sym-constr sym-ctx] (gen/cg-node-raw (n/catch-sym node) class-ctx)
        ;; 顺序处理 body 表达式
        [body-nodes body-constr body-ctx]
        (reduce (fn [[nodes constrs ctx] expr]
                  (let [[_tv new-expr cc new-ctx] (gen/cg-node-raw expr ctx)]
                    [(conj nodes new-expr) (into constrs cc) new-ctx]))
                [[] [] sym-ctx]
                (n/catch-body node))
        tv (gen/fresh-tvar)
        new-node (n/make-catch class-node sym-node (vec body-nodes)
                               (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv)
     (concat class-constr sym-constr body-constr)
     body-ctx]))

(defmethod gen/cg-node-raw :throw [node context]
  (let [[_expr-tv expr-node expr-constr expr-ctx] (gen/cg-node-raw (n/throw-expr node) context)
        tv (gen/fresh-tvar)
        new-node (n/make-throw expr-node
                               (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv)
     expr-constr
     expr-ctx]))
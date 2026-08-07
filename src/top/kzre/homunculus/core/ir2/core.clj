(ns top.kzre.homunculus.core.ir2.core
  "IR2 lowering 核心调度：定义多方法 lower-ast 与入口函数。"
  (:require [top.kzre.homunculus.core.ir1.ast :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.node :as n2]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.internal.protocol :as ip]))

(defrecord Env [ctx])
(defn make-env [ctx] (->Env ctx))
(defn compile-context [env] (:ctx env))
(defn frontend [env] (ip/frontend (compile-context env)))

(defmulti lower-ast
          "将 IR1 节点降低为 IR2 节点向量。"
          (fn [ir1-node _env]
            (ir1/kind ir1-node)))

(defmethod lower-ast :default [_node _env] [])

(defmethod lower-ast :ns [node env]
  (let [frontend (frontend env)
        macro-namespaces (tp/macro-namespaces frontend)
        ;; 过滤宏空间 TODO 上下文合并各种来源的宏空间
        striped-requires (filterv
                           (fn [req]
                             (let [ns-sym (first req)]
                               (not (contains? macro-namespaces ns-sym))))
                           (n1/namespace-requires node))]
    [(n2/make-ns (n1/namespace-name node)
                 striped-requires
                 (n1/namespace-docstring node)
                 {}
                 (n1/node-meta node))]))

(defn lower-nodes
  "对一组 IR1 根节点执行 lowering，返回 IR2 节点向量。"
  [nodes ctx]
  (let [init-env (make-env ctx)]
    (mapcat #(lower-ast % init-env) nodes)))

(ns top.kzre.homunculus.core.types.alpha-rename
  "Alpha 重命名：为所有局部变量生成唯一名称，避免变量捕获。
   使用多方法分派，覆盖所有 IR2 节点类型，全部通过 ir2.node 工具函数操作。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.utils :as u]))

;; ── 动态重命名表（atom）───────────────────
(defmulti rename-node
          (fn [node rename-table] (n/kind node)))

(defmethod rename-node :default [node _rename-table]
  node)

;; ── 叶子 ──────────────────────────────────
(defmethod rename-node :literal [node _] node)

(defmethod rename-node :variable [node rename-table]
  (if-let [new-name (get @rename-table (n/var-name node))]
    (n/make-variable new-name (n/attrs node) (n/node-meta node) (n/parent node))
    node))

;; ── 绑定引入节点 ──────────────────────────
(defmethod rename-node :lambda [node rename-table]
  (let [old-params (n/lambda-params node)
        new-params (mapv (fn [p]
                           (let [old-name (n/var-name p)
                                 new-name (u/fresh-name old-name)]
                             (swap! rename-table assoc old-name new-name)
                             (n/make-variable new-name (n/attrs p) (n/node-meta p) (n/parent p))))
                         old-params)
        new-body (rename-node (n/lambda-body node) rename-table)]
    (n/make-lambda new-params new-body
                   (n/lambda-captures node) (n/lambda-fn-name node)
                   (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod rename-node :let [node rename-table]
  (let [old-bindings (n/let-bindings node)
        new-bindings (mapv (fn [[var val]]
                             (let [old-name (n/var-name var)
                                   new-name (u/fresh-name old-name)]
                               (swap! rename-table assoc old-name new-name)
                               [(n/make-variable new-name (n/attrs var) (n/node-meta var) (n/parent var))
                                (rename-node val rename-table)]))
                           old-bindings)
        new-body (rename-node (n/let-body node) rename-table)]
    (n/make-let new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod rename-node :loop [node rename-table]
  (let [old-bindings (n/loop-bindings node)
        new-bindings (mapv (fn [[var val]]
                             (let [old-name (n/var-name var)
                                   new-name (u/fresh-name old-name)]
                               (swap! rename-table assoc old-name new-name)
                               [(n/make-variable new-name (n/attrs var) (n/node-meta var) (n/parent var))
                                (rename-node val rename-table)]))
                           old-bindings)
        new-body (rename-node (n/loop-body node) rename-table)]
    (n/make-loop new-bindings new-body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod rename-node :catch [node rename-table]
  (let [old-sym (n/catch-sym node)
        old-name (n/var-name old-sym)
        new-name (u/fresh-name old-name)]
    (swap! rename-table assoc old-name new-name)
    (n/make-catch (rename-node (n/catch-class node) rename-table)
                  (n/make-variable new-name (n/attrs old-sym) (n/node-meta old-sym) (n/parent old-sym))
                  (mapv #(rename-node % rename-table) (n/catch-body node))
                  (n/attrs node) (n/node-meta node) (n/parent node))))

;; ── 容器递归 ─────────────────────────────
(defmethod rename-node :call [node rename-table]
  (n/make-call (rename-node (n/call-fn node) rename-table)
               (mapv #(rename-node % rename-table) (n/call-args node))
               (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :if [node rename-table]
  (n/make-if (rename-node (n/if-test node) rename-table)
             (rename-node (n/if-then node) rename-table)
             (when-let [e (n/if-else node)] (rename-node e rename-table))
             (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :block [node rename-table]
  (n/make-block (mapv #(rename-node % rename-table) (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :assign [node rename-table]
  (n/make-assign (rename-node (n/assign-var node) rename-table)
                 (rename-node (n/assign-val node) rename-table)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :recur [node rename-table]
  (n/make-recur (mapv #(rename-node % rename-table) (n/recur-args node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :try [node rename-table]
  (n/make-try (rename-node (n/try-body node) rename-table)
              (mapv #(rename-node % rename-table) (n/try-catches node))
              (when-let [f (n/try-finally node)] (rename-node f rename-table))
              (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :throw [node rename-table]
  (n/make-throw (rename-node (n/throw-expr node) rename-table)
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :while [node rename-table]
  (n/make-while (rename-node (n/while-test node) rename-table)
                (rename-node (n/while-body node) rename-table)
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :vector [node rename-table]
  (n/make-vector (mapv #(rename-node % rename-table) (n/vector-items node))
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :map [node rename-table]
  (n/make-map (mapv #(rename-node % rename-table) (n/map-kvs node))
              (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :define [node rename-table]
  (n/make-define (n/define-name node)
                 (rename-node (n/define-val node) rename-table)
                 (n/define-doc node)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :convert [node rename-table]
  (n/make-convert (rename-node (n/convert-expr node) rename-table)
                  (n/convert-src-ty node) (n/convert-dst-ty node) (n/convert-cost node)
                  (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod rename-node :member-access [node rename-table]
  (n/make-member-access (rename-node (n/access-target node) rename-table)
                        (n/access-member node) ;; accessor 不变
                        (mapv #(rename-node % rename-table) (n/access-args node))
                        (n/node-meta node) (n/parent node)))

(defmethod rename-node :ns [node _] node)
(defmethod rename-node :record [node _] node)
(defmethod rename-node :protocol [node _] node)

;; ── 入口 ──────────────────────────────────
(defn rename
  "对 IR2 节点树执行 alpha 重命名，返回新树。"
  ([node] (rename-node node (atom {})))
  ([node rename-table] (rename-node node rename-table)))
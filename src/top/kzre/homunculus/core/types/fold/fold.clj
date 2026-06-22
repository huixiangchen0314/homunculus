(ns top.kzre.homunculus.core.types.fold.fold
  "常量折叠 Pass：递归遍历 IR2 树，利用后端实现的 IFolder 协议进行常量折叠。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.fold.protocol :as p]))

(defmulti fold-node (fn [node _folder _context] (n/kind node)))

;; ── 叶子节点直接返回 ──
(defmethod fold-node :literal   [node _ _] node)
(defmethod fold-node :variable  [node _ _] node)

;; ── 容器节点递归处理 ──
(defmethod fold-node :if [node folder context]
  (n/make-if (fold-node (n/if-test node) folder context)
             (fold-node (n/if-then node) folder context)
             (when-let [e (n/if-else node)] (fold-node e folder context))
             (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod fold-node :block [node folder context]
  (n/make-block (mapv #(fold-node % folder context) (n/block-exprs node))
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod fold-node :let [node folder context]
  (let [bindings (mapv (fn [[v e]] [(fold-node v folder context) (fold-node e folder context)])
                       (n/let-bindings node))
        body (fold-node (n/let-body node) folder context)]
    (n/make-let bindings body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod fold-node :loop [node folder context]
  (let [bindings (mapv (fn [[v e]] [(fold-node v folder context) (fold-node e folder context)])
                       (n/loop-bindings node))
        body (fold-node (n/loop-body node) folder context)]
    (n/make-loop bindings body (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod fold-node :define [node folder context]
  (if-let [val (n/define-val node)]
    (let [new-val (fold-node val folder context)]
      (n/make-define (n/define-name node) new-val (n/define-doc node)
                     (n/attrs node) (n/node-meta node) (n/parent node)))
    node))

(defmethod fold-node :lambda [node folder context]
  (let [params (mapv #(fold-node % folder context) (n/lambda-params node))
        body (fold-node (n/lambda-body node) folder context)]
    (n/make-lambda params body (n/lambda-captures node) (n/lambda-fn-name node)
                   (n/attrs node) (n/node-meta node) (n/parent node))))

(defmethod fold-node :while [node folder context]
  (n/make-while (fold-node (n/while-test node) folder context)
                (fold-node (n/while-body node) folder context)
                (n/attrs node) (n/node-meta node) (n/parent node)))

(defmethod fold-node :assign [node folder context]
  (n/make-assign (fold-node (n/assign-var node) folder context)
                 (fold-node (n/assign-val node) folder context)
                 (n/attrs node) (n/node-meta node) (n/parent node)))

;; ── 数组特殊节点 ──
(defmethod fold-node :new-array [node folder context]
  (n/make-new-array (fold-node (n/new-array-size node) folder context)
                    (n/node-meta node) (n/parent node)))
(defmethod fold-node :aget [node folder context]
  (n/make-aget (fold-node (n/aget-target node) folder context)
               (fold-node (n/aget-idx node) folder context)
               (n/node-meta node) (n/parent node)))
(defmethod fold-node :aset [node folder context]
  (n/make-aset (fold-node (n/aset-target node) folder context)
               (fold-node (n/aset-idx node) folder context)
               (fold-node (n/aset-val node) folder context)
               (n/node-meta node) (n/parent node)))
(defmethod fold-node :alength [node folder context]
  (n/make-alength (fold-node (n/alength-target node) folder context)
                  (n/node-meta node) (n/parent node)))

;; ── 调用节点：先递归处理子节点，再尝试折叠 ──
(defmethod fold-node :call [node folder context]
  (let [new-fn  (fold-node (n/call-fn node) folder context)
        new-args (mapv #(fold-node % folder context) (n/call-args node))
        temp-node (n/make-call new-fn new-args (n/attrs node) (n/node-meta node) (n/parent node))]
    (or (p/fold-node folder temp-node context)
        temp-node)))

;; ── 其他节点（如 recur, convert, member-access, try, catch, throw, vector, map 等）──
(defmethod fold-node :default [node _ _] node)

;; ── 入口 ──
(defn fold
  "进行一趟代码折叠，折叠是前向的，不需要积累上下文."
  [ir2-roots folder context]
  (mapv #(fold-node % folder context) ir2-roots))
(ns top.kzre.homunculus.core.ir2.forms.call
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

;; 注意，我们不需要专门的 RT 节点。前端协议本身已经承担了 RT 的责任了。
;; ── 原语分发 ──────────────────────────────────────────
(defmulti make-prim
          "根据原语操作符 op 分发到具体的 IR 节点构造。
           参数: [op args node]
           op    – 操作名（符号）
           args  – 已 lowering 的参数向量
           node  – 原始 :call 节点"
          (fn [op _args _node] op))

;; ── 数组原语的具体实现 ────────────────────────────────
(defmethod make-prim '%%aget
  [_ args node]
  (let [[target idx] args]
    [(n2/make-aget target idx {} (n1/node-meta node) nil)]))

(defmethod make-prim '%%aset
  [_ args node]
  (let [[target idx val] args]
    [(n2/make-aset target idx val {} (n1/node-meta node) nil)]))

(defmethod make-prim '%%new-array
  [_ args node]
  (let [[size] args]
    [(n2/make-new-array size {} (n1/node-meta node) nil)]))

(defmethod make-prim '%%alength
  [_ args node]
  (let [[target] args]
    [(n2/make-alength target {} (n1/node-meta node) nil)]))

(defmethod make-prim :default
  [op _ _]
  (throw (ex-info (str "Unknown primitive call: " op) {:op op})))

;; ── 原语集合（自动推导，避免手动维护） ─────────────────
(def ^:private primitive-ops
  (set (keys (methods make-prim))))

;; ── 主 lowering ──────────────────────────────────────
(defmethod ir2/lower-ast :call [node env]
  (let [fn-node (first (ir2/lower-ast (n1/call-op node) env))
        args    (mapv #(first (ir2/lower-ast % env)) (n1/call-args node))]
    (if (and (n2/variable-node? fn-node)
             (contains? primitive-ops (n2/var-name fn-node)))
      (make-prim (n2/var-name fn-node) args node)
      ;; 普通调用
      [(n2/make-call fn-node args {} (n1/node-meta node) nil)])))
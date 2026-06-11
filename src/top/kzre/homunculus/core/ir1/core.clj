;; ═══════════════════════════════════════════
;; ir1/core.clj
;; ═══════════════════════════════════════════
(ns top.kzre.homunculus.core.ir1.core
  "IR1 核心：基于 defrecord 的 AST 节点构造与表单解析。
   所有特殊形式的解析逻辑在 ir1.forms 中。"
  (:require [top.kzre.homunculus.core.ir1.protocol :as p]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defn attach-parents
  "递归设置 node 及其子节点的 parent 指针，返回全新的树。"
  [node parent]
  (let [with-parent (p/set-parent node parent)
        childs      (p/children with-parent)
        new-children (mapv #(attach-parents % with-parent) childs)]
    (if (instance? clojure.lang.IRecord with-parent)
      (clojure.core/assoc with-parent :children new-children)
      (throw (ex-info "Expected record for attach-parents" {})))))

(declare ->ir1)

;; ── 表单 → 节点记录 分派器 ──────────────
(defmulti form->node
          (fn [form]
            (cond
              (or (number? form) (string? form) (true? form) (false? form)
                  (nil? form) (keyword? form) (char? form)) :literal
              (symbol? form) :symbol
              (vector? form) :vector
              (map? form)    :map
              (seq? form)    (if (symbol? (first form)) (first form) :call)
              :else (throw (ex-info (str "Unsupported form: " form) {:form form})))))

;; ── 基础类型 → 记录 ────────────────────
(defmethod form->node :literal [form]
  (m/->LiteralNode form nil [] nil))

(defmethod form->node :symbol [form]
  (m/->SymbolNode form (meta form) [] nil))

(defmethod form->node :vector [form]
  (m/->VectorNode (vec form) nil [] nil))

(defmethod form->node :map [form]
  (m/->MapNode (vec form) nil [] nil))

(defmethod form->node :call [form]
  (let [[op & args] form]
    (m/->CallNode op args nil [] nil)))

(defmethod form->node :default [form]
  (if (seq? form)
    (let [[op & args] form]
      (m/->CallNode op args nil [] nil))
    (throw (ex-info (str "Unsupported form: " form) {:form form}))))

;; ── 树构建器（多方法） ──────────────────
(defmulti build-tree (fn [node] (p/kind node)))

(defmethod build-tree :literal [node] node)
(defmethod build-tree :symbol  [node] node)

(defmethod build-tree :vector [node]
  (let [items (mapv ->ir1 (:items node))]
    (assoc node :children items)))

(defmethod build-tree :map [node]
  (let [pairs (:pairs node)
        kv-nodes (mapcat (fn [[k v]] [(->ir1 k) (->ir1 v)]) pairs)]
    (assoc node :children kv-nodes)))

(defmethod build-tree :call [node]
  (let [op-node (->ir1 (:op node))
        arg-nodes (mapv ->ir1 (:args node))]
    (assoc node :children (into [op-node] arg-nodes))))

;; ── 入口 ─────────────────────────────────
(defn ->ir1 [form]
  (let [raw-node (form->node form)
        tree     (build-tree raw-node)]
    (attach-parents tree nil)))
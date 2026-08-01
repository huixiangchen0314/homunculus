(ns top.kzre.homunculus.core.ir1.core
  "IR1 核心：基于 defrecord 的 AST 节点构造与表单解析。
   所有特殊形式的解析逻辑在 ir1.forms 中。"
  (:require
    [top.kzre.homunculus.core.ir1.model :as m]
   [top.kzre.homunculus.core.ir1.protocol :as p]))

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
              (seq? form)  (let [op (first form)]
                             (cond (symbol? op) op
                                   (keyword? op) :keyword-access ;; 关键字访问
                                   :else :call))
              :else (throw (ex-info (str "Unsupported form: " form) {:form form})))))

(defmethod form->node :default [form]
  (if (seq? form)
    (let [[op & args] form]
      (m/->Call op args nil ))
    (throw (ex-info (str "Unsupported form: " form) {:form form}))))




(defmulti build-tree (fn [node] (p/kind node)))


(defn ->ir1 [form]
  (let [raw-node (form->node form)
        tree     (build-tree raw-node)]
    tree))
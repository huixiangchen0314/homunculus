(ns top.kzre.homunculus.core.ir2
  "将IR1 转换为语言无关的ir2 AST.这是AST还保留高级的语法结构, 如闭包等."
  (:require [clojure.walk :as walk]))

;; ── IR2 节点构造器 ──
(defn- mk-node [kind & kvs]
  (apply assoc {:kind kind} kvs))

(defn literal   [val type]   (mk-node :literal :val val :type type))
(defn var-ref   [sym type]   (mk-node :var :name (name sym) :type type))
(defn prim-call [op args type] (mk-node :prim :op op :type type :args (vec args)))
(defn call      [fn-expr args type] (mk-node :call :fn fn-expr :args (vec args) :type type))
(defn if-expr   [test then else type] (mk-node :if :test test :then then :else else :type type))
(defn do-expr   [exprs type]   (mk-node :do :exprs (vec exprs) :type type))
(defn let-expr  [bindings body type bindings-count] (mk-node :let :bindings bindings :body body :type type :bindings-count bindings-count))
(defn fn-expr   [params body type fn-name] (mk-node :fn :params params :body body :type type :fn-name fn-name))
(defn vector-expr [items type] (mk-node :vector :items (vec items) :type type))
(defn map-expr  [kvs type]    (mk-node :map :kvs (vec kvs) :type type))

;; ── 辅助：类型推断占位（实际应由类型检查 pass 填充）──
(defn- infer-type [form] {:kind :any})

;; ── Multimethod 分发：根据节点类型 ──
(defmulti lower-ast
          "将 Clojure AST 节点降低为 IR2 节点向量。返回 IR2 向量。"
          (fn [form env]
            (cond
              (and (seq? form) (symbol? (first form))) (first form)  ;; 符号分发
              (vector? form) :vector
              (map? form)    :map
              (symbol? form) :symbol
              (keyword? form) :keyword
              (number? form) :number
              (string? form) :string
              (nil? form)    :nil
              :else          :unknown)))

;; ══════════════════════════════════════════════
;; 各方法的实现
;; ══════════════════════════════════════════════

(defmethod lower-ast 'def [form env]
  ;; (def name value) 或 (def name (fn ...))
  (let [sym (second form)
        val (nth form 2 nil)
        lowered-val (when val (lower-ast val env))
        kind (if (and lowered-val (= (::kind (first lowered-val)) :fn))
               :def-fn
               :def-var)
        type (infer-type form)]
    [(mk-node kind :name sym :value lowered-val :type type)]))

(defmethod lower-ast 'fn [form env]
  (let [params (second form)
        body (drop 2 form)
        lowered-body (doall (map #(lower-ast % env) body))
        type (infer-type form)
        fn-name (gensym "fn")]   ;; 匿名函数用临时名
    [(fn-expr params lowered-body type fn-name)]))

(defmethod lower-ast 'if [form env]
  (let [test (lower-ast (second form) env)
        then (lower-ast (nth form 2) env)
        else (when (> (count form) 3) (lower-ast (nth form 3) env))
        type (infer-type form)]
    [(if-expr test then else type)]))

(defmethod lower-ast 'do [form env]
  (let [exprs (map #(lower-ast % env) (rest form))
        type (infer-type form)]
    [(do-expr exprs type)]))

(defmethod lower-ast 'let [form env]
  (let [bindings (second form)
        pairs (partition 2 bindings)
        body (drop 2 form)
        lowered-body (doall (map #(lower-ast % env) body))
        bindings-count (count pairs)
        type (infer-type form)]
    [(let-expr bindings lowered-body type bindings-count)]))

(defmethod lower-ast 'loop [form env]
  ;; loop 类似 let，额外标记为循环
  (let [bindings (second form)
        pairs (partition 2 bindings)
        body (drop 2 form)
        lowered-body (doall (map #(lower-ast % env) body))
        bindings-count (count pairs)
        type (infer-type form)]
    [(mk-node :loop :bindings bindings :body lowered-body :type type :bindings-count bindings-count)]))

(defmethod lower-ast 'recur [form env]
  (let [args (map #(lower-ast % env) (rest form))
        type (infer-type form)]
    [(mk-node :recur :args (vec args) :type type)]))

(defmethod lower-ast 'quote [form env]
  [(literal (second form) {:kind :any})])

;; ── 原始字面量 ──
(defmethod lower-ast :number [form env]
  [(literal form {:kind :prim, :name (type form)})])

(defmethod lower-ast :string [form env]
  [(literal form {:kind :prim, :name :string})])

(defmethod lower-ast :keyword [form env]
  [(literal form {:kind :prim, :name :keyword})])

(defmethod lower-ast :nil [form env]
  [(literal nil {:kind :void})])

(defmethod lower-ast :symbol [form env]
  (let [sym form
        type (infer-type form)]
    [(var-ref sym type)]))

;; ── 集合字面量 ──
(defmethod lower-ast :vector [form env]
  (let [items (map #(lower-ast % env) form)
        type (infer-type form)]
    [(vector-expr items type)]))

(defmethod lower-ast :map [form env]
  (let [kvs (mapcat (fn [[k v]] [(lower-ast k env) (lower-ast v env)]) form)
        type (infer-type form)]
    [(map-expr (vec kvs) type)]))

;; ── 函数调用（任何其他列表）──
(defmethod lower-ast :default [form env]
  (if (seq? form)
    (let [f (lower-ast (first form) env)
          args (map #(lower-ast % env) (rest form))
          type (infer-type form)]
      [(call f args type)])
    (throw (ex-info (str "Unknown form in IR2 lowering: " form) {}))))

;; ── 顶层入口：处理多个顶层表单 ──
(defn lower-top [forms]
  (let [env {} ;; 环境可以存放类型上下文等
        lowered (mapcat #(lower-ast % env) forms)]
    lowered))
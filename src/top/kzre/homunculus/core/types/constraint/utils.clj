(ns top.kzre.homunculus.core.types.constraint.utils
  "约束系统上下文访问与操作工具函数。
   纯数据访问，不依赖任何约束生成逻辑，可安全被 gen.core、solve 等模块使用。"
  (:require
   [top.kzre.homunculus.core.types.env :as e]
   [top.kzre.homunculus.core.types.protocol :as p]))

;; ── 原始访问器 ──────────────────────────
(defn frontend [ctx] (:frontend ctx))
(defn backend [ctx] (:backend ctx))
(defn compile-ctx [ctx] (:ctx ctx))
(defn env [ctx] (:env ctx))
(defn symbol-table [ctx] (:symbol-table ctx))
(defn known-types [ctx] (:known-types ctx))

;; ── 环境工具 ────────────────────────────
(defn lookup-env
  "在上下文的环境中查找变量名，返回绑定的类型或 nil。"
  [ctx name]
  (e/lookup-env (env ctx) name))

;; ── 上下文更新器 ─────────────────────────
(defn new-env
  "返回一个更新了环境的新上下文。"
  [ctx new-env]
  (assoc ctx :env new-env))

(defn extend-env
  "在上下文的环境中绑定变量名与类型，返回新上下文。"
  [ctx name ty]
  (new-env ctx (e/extend-env (env ctx) name ty)))

(defn add-known-type
  "向上下文的已知类型集合中添加一个类型符号。"
  [ctx type-sym]
  (update ctx :known-types conj type-sym))

;; ── 约束策略访问 ─────────────────────────
(defn constraint-policy
  "从上下文中提取约束策略协议实例。"
  [ctx]
  (:constraint-policy ctx))
(defn truthy-type-requirement [ctx]
  "从上下文中获取前端的真值类型要求。"
  (p/truly-type (frontend ctx)))

(defn dynamic-branch-types? [ctx]
  "从上下文中获取前端是否支持动态分支类型。"
  (p/dynamic? (frontend ctx)))
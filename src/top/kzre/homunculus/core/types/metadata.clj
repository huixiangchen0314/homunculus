(ns top.kzre.homunculus.core.types.metadata
  "IR2 节点元数据读取工具。
   提供便捷函数用于访问函数、参数、定义等各层级的元数据，
   以及元数据中常见的标记判断。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

;; ── 基础工具（已存在）─────────────────────
(defn node-meta
  "安全获取节点的元数据 map，若节点不满足 INode 则返回 nil。"
  [node]
  (when (satisfies? ir2p/INode node)
    (ir2p/node-meta node)))

(defn has-meta?
  "节点元数据中是否包含键 k。"
  [node k]
  (boolean (some-> node node-meta (contains? k))))

(defn get-meta
  "从节点元数据中读取键 k 对应的值，若不存在则返回 nil。"
  [node k]
  (some-> node node-meta (get k)))

;; ── 元数据标记 ────────────────────────────
(defn meta-flag?
  "元数据中是否存在关键字 k（值为 true），用于判断如 ^:export 等标记。"
  [node k]
  (true? (get-meta node k)))

;; ── 函数元数据 ─────────────────────────────
(defn fn-params
  "返回函数节点（LambdaNode）的参数列表。"
  [fn-node]
  (n/lambda-params fn-node))

(defn param-meta
  "获取函数参数 param-node（VariableNode）的元数据 map。"
  [param-node]
  (node-meta param-node))

(defn fn-return-tag
  "从函数（LambdaNode）的元数据中提取 :tag，通常为用户标注的返回类型。"
  [fn-node]
  (get-meta fn-node :tag))

(defn fn-shader-stage
  "从函数元数据中获取着色器阶段（如 :vertex, :pixel）。"
  [fn-node]
  (get-meta fn-node :shader-stage))

(defn fn-entry?
  "函数是否为入口点（元数据 :entry? 为 true）。"
  [fn-node]
  (meta-flag? fn-node :entry?))

;; ── 定义元数据 ─────────────────────────────
(defn def-meta
  "获取 define 节点的元数据。"
  [def-node]
  (node-meta def-node))

(defn def-export?
  "define 节点是否标记为导出。"
  [def-node]
  (meta-flag? def-node :export))

;; ── 参数元数据遍历 ─────────────────────────
(defn params-with-meta
  "返回函数参数中满足谓词 p? 的参数列表。p? 接收参数元数据 map。"
  [fn-node p?]
  (filter #(p? (param-meta %)) (fn-params fn-node)))

;; ── 通用元数据查询 ─────────────────────────
(defn meta-value
  "通用读取：尝试从节点元数据中依次查找 keys（关键字序列），返回第一个存在的值。"
  [node & keys]
  (some #(get-meta node %) keys))
(ns top.kzre.homunculus.core.ir1.pass.expand-symbols
  "IR1 符号展开 Pass：将 IR1 中的符号引用转换为全限定名。

   预处理阶段（ir1.preprocess）只在 s 表达式的 op 位置展开符号——
   仅用于宏展开。本 pass 负责展开其它位置的符号引用：
   - :symbol 节点的 :name
   - 节点元数据中的符号值（当前处理 :tag）

   在宏展开之后、IR1 进一步处理之前运行。
   ns-info 由 ir1.preprocess 产出的 NsInfo 记录提供。

   符号限定是尽力而为的：
   - 已限定（含 namespace）的符号原样保留；
   - 无法限定的符号静默保留——它们可能代表局部绑定、
     稍后处理的宏、或尚未定义的符号。"
  (:require
    [top.kzre.homunculus.core.ir1.ast :as ir1]
    [top.kzre.homunculus.core.ir1.node :as n]
    [top.kzre.homunculus.core.ir1.expand-symbols :as ex]))

;; ── 符号限定 ─────────────────────────────

(defn- qualify-safely
  "尝试用 ns-info 限定 sym。无法限定时返回原符号。
   与 ex/expand-sym 的区别是不抛异常。"
  [sym ns-info]
  (if (or (nil? sym) (namespace sym))
    sym
    (try
      (ex/expand-sym sym ns-info)
      (catch Exception _
        sym))))

;; ── 元数据展开 ───────────────────────────

(defn- expand-meta
  "展开元数据中的符号引用。当前处理 :tag（类型提示）。
   如需支持更多键，在此扩展。"
  [meta ns-info]
  (if-let [tag (:tag meta)]
    (if (symbol? tag)
      (assoc meta :tag (qualify-safely tag ns-info))
      meta)
    meta))

;; ── 单节点展开 ───────────────────────────

(declare expand-node)

(defn- expand-self
  "处理本节点自身的符号与元数据——子节点已由 reduce-children 处理。"
  [node ns-info]
  (let [node (if (= :symbol (ir1/kind node))
               (let [name (:name node)]
                 (if (symbol? name)
                   (assoc node :name (qualify-safely name ns-info))
                   node))
               node)]
    (if (n/node-meta node)
      (update node :meta expand-meta ns-info)
      node)))

(defn- expand-node
  [node ns-info]
  (let [[new-node _] (ir1/reduce-children
                       node
                       (fn [child env]
                         [(expand-node child ns-info) env])
                       nil)]
    (expand-self new-node ns-info)))

;; ── 入口 ─────────────────────────────────

(defn expand
  "展开 IR1 根节点列表中的符号引用。
   ns-info 为 ir1.preprocess 产出的 NsInfo 记录。"
  [ir1-roots ns-info]
  (mapv #(expand-node % ns-info) ir1-roots))
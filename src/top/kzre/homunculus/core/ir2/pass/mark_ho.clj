(ns top.kzre.homunculus.core.ir2.pass.mark-ho
  "高阶函数标记 Pass：为高阶 lambda 的 define 打 :ho? 标记。

   通用 pass——不绑定任何具体消费者。
   两个入口：
   - mark      遍历全树，批处理
   - mark-node 单个节点，供产生新节点的 pass 增量使用"
  (:require
    [top.kzre.homunculus.core.ir2.analysis.ho :as ho]
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]))

(defn- high-order? [node]
  (if (= :lambda (ir2/kind node))
    (ho/ho? node)
    false))

(declare walk)

(defn- mark-fn
  "处理单个节点：如果是 define 且其值为 lambda，则根据高阶检测设置 :ho? 属性。
   其他节点原样返回。"
  [node env]
  (if (= :define (ir2/kind node))
    (let [new-val (first (mark-fn (n/define-val node) env))
          ho?     (high-order? new-val)]
      [(n/make-define (n/define-name node)
                      new-val
                      (n/define-docstring node)
                      (if ho?
                        (assoc (n/attrs node) :ho? true)
                        (n/attrs node))
                      (n/node-meta node))
       env])
    (walk node env)))

(defn- walk
  [node env]
  (ir2/reduce-children node mark-fn env))

;; ── 对外入口 ─────────────────────────────
(defn mark-node
  "对单个节点按 kind 分派打 :ho? 标记。
   未支持的 kind 原样返回——调用方不需要判断节点类型。"
  [node]
  (first (mark-fn node nil)))

(defn mark
  "遍历 IR2 根节点，为高阶 lambda 的 define 打 :ho? 标记。"
  [ir2-roots]
  (mapv mark-node ir2-roots))
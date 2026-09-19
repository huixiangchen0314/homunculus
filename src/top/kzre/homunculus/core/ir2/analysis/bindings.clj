(ns top.kzre.homunculus.core.ir2.analysis.bindings
  "从绑定节点提取绑定名。

   纯查询——只读节点，返回名字集合或序列。
   被多个 pass 复用（free-vars / rename / lambda-elim / ...）。

   严格模式：非 IR2 节点报错。"
  (:require
    [top.kzre.homunculus.core.ir2.ast :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]))

;; ═══════════════════════════════════════════════
;; 约束
;; ═══════════════════════════════════════════════

(defn- ensure-ir2!
  [node where]
  (when-not (ir2/ir2? node)
    (throw (ex-info (str where " expects an IR2 node")
                    {:node node
                     :type (type node)}))))

(defn- ensure-kind!
  [node expected where]
  (ensure-ir2! node where)
  (when-not (= expected (n/kind node))
    (throw (ex-info (str where " expects a " expected " node")
                    {:node node
                     :kind (n/kind node)}))))

;; ═══════════════════════════════════════════════
;; 绑定名提取
;; ═══════════════════════════════════════════════

(defn let-names
  "从 let 节点提取所有绑定名。返回集合。"
  [let-node]
  (ensure-kind! let-node :let "let-names")
  (into #{} (map #(:name (:var %))) (n/let-bindings let-node)))

(defn loop-names
  "从 loop 节点提取所有绑定名。返回集合。"
  [loop-node]
  (ensure-kind! loop-node :loop "loop-names")
  (into #{} (map #(:name (:var %))) (n/loop-bindings loop-node)))

(defn lambda-names
  "从 lambda 节点提取所有参数名。返回集合。"
  [lambda-node]
  (ensure-kind! lambda-node :lambda "lambda-names")
  (into #{} (map :name) (n/lambda-params lambda-node)))

(defn catch-names
  "从 catch 节点提取异常符号名。返回集合（单元素）。"
  [catch-node]
  (ensure-kind! catch-node :catch "catch-names")
  #{(:name (n/catch-sym catch-node))})

;; ═══════════════════════════════════════════════
;; 通用入口
;; ═══════════════════════════════════════════════

(defn binding-names
  "节点自身引入的绑定名集合——不含子节点。

   适用节点：:let / :loop / :lambda / :catch。
   其他节点返回空集。"
  [node]
  (ensure-ir2! node "binding-names")
  (case (n/kind node)
    :let    (let-names    node)
    :loop   (loop-names   node)
    :lambda (lambda-names node)
    :catch  (catch-names  node)
    #{}))
(ns top.kzre.homunculus.core.ir2.analysis.free-vars
  "自由变量分析。

   自由变量 = 在子树中被引用，但未在子树内任何绑定位置引入的变量。

   两个入口：
   - free-vars            —— 任意 IR2 节点的自由变量
   - free-vars-of-lambda  —— lambda 节点的自由变量（排除参数）

   独立于任何 pass——不读 attrs 上的临时标记，不写回 IR。
   不假设 captures 字段的存在——直接从 AST 计算。

   严格模式：
   - 非 IR2 节点        —— 报错
   - 非 lambda 传 free-vars-of-lambda —— 报错"
  (:require
    [clojure.set :as set]
    [top.kzre.homunculus.core.ir2.analysis.bindings :as bindings]
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

(defn- ensure-lambda!
  [node where]
  (ensure-ir2! node where)
  (when-not (= :lambda (n/kind node))
    (throw (ex-info (str where " expects a lambda node")
                    {:node node
                     :kind (n/kind node)}))))

;; ═══════════════════════════════════════════════
;; 内部：收集
;; ═══════════════════════════════════════════════

(defn- collect-bound
  "返回 node 子树内所有被 let / lambda / loop / catch 引入的变量名集合。"
  [node]
  (ensure-ir2! node "collect-bound")
  (let [here     (bindings/binding-names node)
        children (into #{} (mapcat collect-bound) (n/children node))]
    (into here children)))

(defn- collect-free
  "在已知 bound 集合下，返回 node 子树中所有未绑定变量名的集合。"
  [bound node]
  (ensure-ir2! node "collect-free")
  (if (= :variable (n/kind node))
    (let [name (:name node)]
      (if (contains? bound name) #{} #{name}))
    (into #{} (mapcat #(collect-free bound %)) (n/children node))))

;; ═══════════════════════════════════════════════
;; 对外入口
;; ═══════════════════════════════════════════════

(defn free-vars
  "返回 node 子树中所有自由变量名。
   自由变量 = 在子树内被引用，但未在子树内任何绑定位置引入。"
  [node]
  (ensure-ir2! node "free-vars")
  (let [bound (collect-bound node)
        free  (collect-free bound node)]
    free))

(defn free-vars-of-lambda
  "返回 lambda 节点的自由变量名集合。
   lambda 的参数是本地绑定——从自由变量中排除。"
  [lam]
  (ensure-lambda! lam "free-vars-of-lambda")
  (let [param-names (bindings/lambda-names lam)]
    (set/difference (free-vars (:body lam)) param-names)))
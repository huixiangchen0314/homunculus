(ns top.kzre.homunculus.core.irstmt.pass.declare.strip
  "VarDecl → Assign——为 CFG 构造准备。

   CFG 只关心 def/use。VarDecl 的 name 是 def，val 是 def 的
   rhs——转成 Assign 让 CFG 的 stmt-defs 能识别。

   ▸ 带初值的 VarDecl → Assign(variable(name), val)
   ▸ 无初值的 VarDecl → 删除（纯声明，无 def）

   递归进 Block / If / While / Function / Method 的 body。"
  (:require [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defn- strip-stmt
  [stmt]
  (case (ast/kind stmt)
    :var-decl
    (when-let [val (:val stmt)]
      (ast/->Assign
        (ast/->Variable (:name stmt) (ast/attrs stmt) (ast/node-meta stmt))
        val
        (ast/attrs stmt)
        (ast/node-meta stmt)))
    :block
    (-> stmt
        (update :stmts #(vec (keep strip-stmt %)))
        (update :ret identity))
    :if
    (-> stmt
        (update :then strip-stmt)
        (update :else #(when % (strip-stmt %))))
    :while
    (update stmt :body strip-stmt)
    :function
    (update stmt :body strip-stmt)
    :method
    (update stmt :body strip-stmt)
    stmt))

(defn strip-node [node]
  (strip-stmt node))

(defn strip
  [nodes]
  (mapv strip-stmt nodes))
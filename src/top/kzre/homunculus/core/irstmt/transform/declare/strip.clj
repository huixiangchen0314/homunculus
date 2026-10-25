(ns top.kzre.homunculus.core.irstmt.transform.declare.strip
  "VarDecl → Assign——为 CFG 构造准备。

   CFG 只关心 def/use。VarDecl 的 name 是 def，val 是 def 的
   rhs——转成 Assign 让 CFG 的 stmt-defs 能识别。

   ▸ 带初值的 VarDecl → Assign(variable(name), val)
   ▸ 无初值的 VarDecl → nil（删除）

   只一个 :var-decl 的 defmethod，其余全走 :default 的
   rreduce-children——defast 生成的遍历自动覆盖所有子节点：
   block 的 stmts / ret、if 的 test / then / else、while 的
   test / body、表达式节点的所有 children。"
  (:require
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defmulti strip-node
          (fn [node _env] (ast/kind node)))

(defmethod strip-node :var-decl
  [stmt _env]
  (if-let [val (:val stmt)]
    [(ast/->Assign
       (ast/->Variable (:name stmt) (ast/attrs stmt) (ast/node-meta stmt))
       val
       (ast/attrs stmt)
       (ast/node-meta stmt))
     nil]
    ;;reduce-children 过滤nil
    [nil nil]))

(defmethod strip-node :default
  [node env]
  (ast/reduce-children node strip-node env))

(defn strip-function
  [node]
  (let [[body' _] (strip-node (:body node) nil)]
    (assoc node :body body')))
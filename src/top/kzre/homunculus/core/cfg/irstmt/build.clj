(ns top.kzre.homunculus.core.cfg.irstmt.build
  "IRStmt 的 ICFGBuilder 实现。

   block-form 是内部辅助——只被 decon 调用。规范形式里带 :origin
   字段（原 AST 节点），供 build 侧设置 CFG BasicBlock 的 origin。

   非 :block 节点——:origin 就是节点本身。
   :block 节点——:origin 是 :block 节点本身（它的 stmts 会展开到
   当前 CFG 块，:origin 用于 bake 时给 Block 节点继承 attrs）。"
  (:require
    [top.kzre.homunculus.core.cfg.build :as build]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

(defn- block-form
  "IRStmt 的 :block 节点 → 规范 block 形式（含 :origin）。

   ▸ :block 节点 → {:kind :block :stmts [...] :ret expr :origin node}
   ▸ 裸表达式    → {:kind :block :stmts [] :ret node :origin node}
   ▸ nil         → {:kind :block :stmts [] :ret nil :origin nil}"
  [node]
  (cond
    (nil? node)
    {:kind :block :stmts [] :ret nil :origin nil}

    (= :block (ast/kind node))
    {:kind   :block
     :stmts  (vec (:stmts node))
     :ret    (:ret node)
     :origin node}

    :else
    {:kind   :block
     :stmts  []
     :ret    node
     :origin node}))

(defrecord IRStmtCFGBuilder []
  build/ICFGBuilder
  (decon [_ node]
    (case (ast/kind node)
      :if    {:kind   :if
              :test   (:test node)
              :then   (block-form (:then node))
              :else   (when (:else node) (block-form (:else node)))
              :origin node}                              ; ← if 节点
      :while {:kind   :while
              :test   (:test node)
              :body   (block-form (:body node))
              :origin node}                              ; ← while 节点
      :throw {:kind   :throw
              :expr   (:expr node)
              :origin node}
      :block (block-form node)
      {:kind   :stmt
       :origin node})))

(defonce ^:private builder* (->IRStmtCFGBuilder))

(defn builder [] builder*)
(ns top.kzre.homunculus.core.cfg.irstmt.destruct
  "IRStmt 的 phi 消除便捷入口。"
  (:require
    [top.kzre.homunculus.core.cfg.pass.ssa.destruct :as destruct]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))


(defn- make-move
  "构造 IRStmt 的 assign 节点：var-name = value。"
  [var-name value]
  (ast/->Assign
    (ast/->Variable var-name {} nil)
    value
    {}
    nil))


(defn elim-phi
  [ssa-cfg]
  (destruct/elim-phi ssa-cfg make-move))
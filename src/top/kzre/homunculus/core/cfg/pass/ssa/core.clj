(ns top.kzre.homunculus.core.cfg.pass.ssa.core
  "SSA 全流程编排：construct → copy-prop → destruct。"
  (:require
    [top.kzre.homunculus.core.cfg.pass.ssa.construct :as construct]
    [top.kzre.homunculus.core.cfg.pass.ssa.copy-prop :as copy-prop]
    [top.kzre.homunculus.core.cfg.pass.ssa.destruct  :as destruct]))

(defn run
  "CFG → SSA → copy-prop → 普通 CFG。

   adapter     —— IVarAccess 实例
   make-move-fn —— (fn [var-name value] → assign-stmt)
                   phi 消除方法 2 的 move 构造器"
  [cfg adapter make-move-fn]
  (let [ssa       (construct/construct cfg adapter)
        optimized (copy-prop/copy-prop ssa adapter)
        restored  (destruct/elim-phi optimized make-move-fn)]
    restored))
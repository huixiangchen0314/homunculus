(ns top.kzre.homunculus.core.cfg.pass.ssa.core
  "SSA 全流程：construct → copy-prop → destruct。"
  (:require
    [top.kzre.homunculus.core.cfg.pass.ssa.construct :as construct]
    [top.kzre.homunculus.core.cfg.pass.ssa.copy-prop :as copy-prop]
    [top.kzre.homunculus.core.cfg.pass.ssa.destruct  :as destruct]))

(defn run
  "CFG → SSA → copy-prop → 普通 CFG。

   adapter 携带所有前端相关操作——包括 make-var-ref / make-move。
   不再需要单独的 make-move-fn 参数。"
  [cfg adapter]
  (let [ssa       (construct/construct cfg adapter)
        optimized (copy-prop/copy-prop ssa adapter)
        restored  (destruct/elim-phi optimized adapter)]
    restored))
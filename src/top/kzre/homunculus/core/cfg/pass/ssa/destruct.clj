(ns top.kzre.homunculus.core.cfg.pass.ssa.destruct
  "Phi 消除——SSA-CFG → 普通 CFG。

   ── 方法 2（phi 定义下推，迭代到不动点）──

   收集所有 phi 的替换 {input-var-name → phi-var-name}，
   全局替换 stmt 的 def lhs，重复直到 CFG 不变。

   ── 为什么需要多轮 ──

   phi 之间有依赖：B 的 input 是 A 的 var-name。第 1 轮时
   A 的 var-name 的 def 还不存在（A 还没下推）——B 的替换
   空转。第 1 轮把 A 的 input X 的 def 改成 A_var——第 2 轮
   B 的替换 {A_var B_var} 命中——A_var 的 def 改成 B_var。
   逐层，直到所有 def 都是最外层 phi 的 var-name。

   循环 phi（input 是自己）——{self self} 替换无变化——收敛。

   ── 依赖 ──

   只用 cfg.adapt / cfg.model / cfg.phi 协议——不依赖
   irstmt.ast。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.model :as cfg])
  (:import (top.kzre.homunculus.core.cfg.model CFG)))

(defn- transform-phi-to-version-def
  [^CFG cfg adapter]
  (let [substs (cfg/collect-phi-substs cfg)]
    (adapt/subtitle adapter cfg substs)))

(defn elim-phi
  "消除所有 phi 节点——方法 2：phi 定义下推，迭代到不动点。

   每轮收集所有 phi 的 {input-var-name → phi-var-name}，
   全局替换 stmt 的 def lhs。CFG 不变时停止。

   循环 phi 的 {self self} 替换无变化——收敛。
   最多 1000 轮——防御性上限。"
  [^CFG ssa-cfg adapter]
  (loop [cfg ssa-cfg
         iteration 0]
    (when (> iteration 1000)
      (throw (ex-info "elim-phi did not converge" {:iterations iteration})))
    (let [cfg' (transform-phi-to-version-def cfg adapter)]
      (if (= cfg cfg')
        cfg'
        (recur cfg' (inc iteration))))))
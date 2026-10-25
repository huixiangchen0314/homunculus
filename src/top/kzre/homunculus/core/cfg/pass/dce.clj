(ns top.kzre.homunculus.core.cfg.pass.dce
  "基于后向活跃分析的 CFG 级死代码消除。

   ── 为什么 CFG 级 ──

   IRStmt DCE 跑在 SSA 形式（destruct 之前）——phi input 的
   变量被 use——不能删。`ifval9851` 是 while head 的 phi 的
   input——destruct 之前是“活”的。

   CFG 级 DCE 跑在 destruct 之后——phi 已下推——`ifval9851`
   不再被 use——能删。块级 liveness 跨块精确——if 分支的
   live 合并由数据流分析处理。

   ── 算法 ──

   1. 计算 CFG 的 liveness
   2. 每个块的 stmts 从后往前：
      ▸ defs 与 live 有交集 → 保留
      ▸ 有副作用 → 保留
      ▸ 否则 → 删除
   3. 有删除 → 重新计算 liveness → 迭代

   ── 输入 ──

   destruct 之后的 CFG——无 phi，stmts 只有 assign 等。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.analysis.liveness :as liveness])
  (:import (top.kzre.homunculus.core.cfg.model CFG)))

;; ═══════════════════════════════════════════
;; 单块删除
;; ═══════════════════════════════════════════

(defn- stmt-alive?
  [adapter stmt live]
  (or (adapt/stmt-fx? adapter stmt)
      (some #(contains? live %) (adapt/stmt-defs adapter stmt))))

(defn- elim-block-stmts
  "从后往前删除块内死 stmt。返回 [stmts' changed?]。"
  [adapter stmts block-live-out]
  (let [{:keys [kept changed?]}
        (reduce
          (fn [{:keys [kept live changed?]} stmt]
            (if (stmt-alive? adapter stmt live)
              (let [uses  (adapt/stmt-uses adapter stmt)
                    defs  (adapt/stmt-defs adapter stmt)
                    live' (into uses (remove defs live))]
                {:kept     (cons stmt kept)
                 :live     live'
                 :changed? changed?})
              {:kept     kept
               :live     live
               :changed? true}))
          {:kept [] :live block-live-out :changed? false}
          (reverse stmts))]
    [(vec kept) changed?]))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn elim
  [^CFG cfg adapter]
  (loop [cfg cfg n 0]
    (when (> n 1000)
      (throw (ex-info "dce did not converge" {:iterations n})))
    (let [live-analysis (liveness/cfg-liveness cfg adapter)
          [cfg' changed?]
          (reduce-kv
            (fn [[c changed?] block-id block]
              (let [live-out (liveness/live-out-set live-analysis block-id)
                    [stmts' block-changed?]
                    (elim-block-stmts adapter (:stmts block) live-out)]
                [(assoc-in c [:blocks block-id :stmts] stmts')
                 (or changed? block-changed?)]))
            [cfg false]
            (:blocks cfg))]
      (if changed?
        (recur cfg' (inc n))
        cfg'))))
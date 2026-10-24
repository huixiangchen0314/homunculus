(ns top.kzre.homunculus.core.cfg.pass.ssa.destruct
  "Phi 消除——SSA-CFG → 普通 CFG。

   ── 阶段 1（往返验证）只用方法 2 ──────────────────────

   方法 2（并行复制）：各前驱末尾插入 phi-var = phi-input 的 move。
   块数和边结构完全不变——只有 stmts 增加了 move 序列。便于验证
   往返正确性。

   方法 1（后继是 return → 各前驱 return phi-input）在阶段 2 加。
   它会删除 phi 块——结构变化，需要更仔细的验证。

   ── 并行复制的正确性 ─────────────────────────────────

   SSA 的 phi 输入按前驱块独立填——不同前驱的 move 互不干扰。
   同一前驱同时喂多个 phi 时，可能存在\"交叉依赖\"——但 SSA 的 phi
   输入来自前驱边上的值，本身不形成循环。每个 move 的顺序不影响
   正确性（emit 时 GLSL 编译器会处理）。

   如果将来遇到需要临时变量的并行复制（phi1.var = phi2.input 且
   phi2.var = phi1.input 在同一前驱），这里会出错——但当前场景不
   出现，暂不处理。"
  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.phi :as phi]))


(defn- eliminate-phi-block
  "对单个块的 phis 执行方法 2——各前驱末尾插入 move。"
  [cfg id make-move-fn]
  (let [block (cfg/cfg-block cfg id)
        phis  (:phis block)]
    (if (empty? phis)
      cfg
      (reduce
        (fn [c p]
          (let [var (phi/phi-var p)]
            (reduce
              (fn [c {:keys [value pred]}]
                (let [move (make-move-fn var value)]
                  (update-in c [:blocks pred :stmts] conj move)))
              c
              (phi/phi-inputs p))))
        cfg
        phis))))


(defn elim-phi
  "消除所有 phi 节点。

   make-move-fn —— (fn [var-name value-expr] → assign-stmt)
                   构造 \"var-name = value-expr\" 的前端赋值节点。

   返回普通 CFG——:phis 字段全部清除，move 序列插入到前驱块末尾。"
  [ssa-cfg make-move-fn]
  (let [ids  (keys (:blocks ssa-cfg))
        cfg' (reduce (fn [c id]
                       (eliminate-phi-block c id make-move-fn))
                     ssa-cfg
                     ids)]
    (update cfg' :blocks
            (fn [blocks]
              (into {}
                    (map (fn [[id b]] [id (assoc b :phis nil)]))
                    blocks)))))
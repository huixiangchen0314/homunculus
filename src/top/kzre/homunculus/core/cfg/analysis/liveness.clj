(ns top.kzre.homunculus.core.cfg.analysis.liveness
  "后向活跃性分析。

   ── 数据流方程 ──────────────────────────────────────

   live-in[B]  = use[B] ∪ (live-out[B] - def[B])
   live-out[B] = ∪ live-in[succ]  （对所有后继）

   终止块（:return / :throw）无后继——live-out 是 ∅。
   不可达块的 live-in / live-out 是 ∅（构造 CFG 时已丢弃不可达代码，
   正常情况下不会出现）。

   ── worklist 算法 ────────────────────────────────────

   按逆后序（reverse postorder）初始化 worklist——后向数据流的传播
   方向与逆后序一致（从出口往上，沿回边传播）。通常一轮迭代就接近
   收敛，比\"每轮遍历所有块\"快。

   当某块的 live-in / live-out 变化时，把它的所有前驱加入 worklist。
   队列里可以有重复——重复计算只是多算一次，不影响正确性，也不影响
   收敛（变化量是单调递减的）。

   ── 初始状态 ─────────────────────────────────────────

   live-in[B]  初始化为 use[B]——这是 live-in 的下界（live-in 一定
   包含 use，因为块内的 use 不需要块外提供，但块外的 live-out 会
   引入额外的活跃变量）。
   live-out[B] 初始化为 ∅。

   初始用 use 而非 ∅ 是收敛加速——live-in 的最终值包含 use，
   从 use 出发只需向上传播 live-out 的贡献。

   ── 与 phi 的关系 ────────────────────────────────────

   本实现**在 phi 插入之前**计算——CFG 没有 phi。用作 pruned SSA
   的判据：变量 x 在块 d 处需要 phi，当且仅当 x ∈ live-in[d]。

   如果需要 SSA-CFG 上的 liveness（比如 coalescing 判断），需要
   扩展 block-uses-defs 处理 :phis 字段——当前不支持。"
  (:require
    [top.kzre.homunculus.core.cfg.analysis.use-def :as use-def]
    [top.kzre.homunculus.core.cfg.model :as cfg]))

;; ═══════════════════════════════════════════
;; 逆后序
;; ═══════════════════════════════════════════

(defn- rpo
  "从 entry 做 DFS，返回 reverse postorder。

   与 dominator.clj 的 dfs-postorder 独立——两者都需要此遍历，
   但计算目的不同（liveness 只用顺序，dominator 还用 dfs-order
   索引做 intersect 快速收敛）。独立实现避免模块间耦合。"
  [cfg]
  (let [visited (atom #{})
        post    (atom [])]
    (letfn [(visit [id]
              (when-not (contains? @visited id)
                (swap! visited conj id)
                (doseq [succ (cfg/cfg-successors-of cfg id)]
                  (visit succ))
                (swap! post conj id)))]
      (visit (:entry cfg)))
    (vec (reverse @post))))

;; ═══════════════════════════════════════════
;; 主计算
;; ═══════════════════════════════════════════

(defn compute-liveness
  "计算后向活跃性。

   cfg     —— CFG（普通，无 phi）
   adapter —— IVarAccess 实例

   返回 {:live-in  {block-id → #{var...}}
         :live-out {block-id → #{var...}}}

   查询 live-in / live-out 用 get-in 或辅助函数 live-in? / live-out?。"
  [cfg adapter]
  (let [order      (rpo cfg)
        preds      (cfg/cfg-build-preds cfg)
        succs      (cfg/cfg-build-succs cfg)
        block-info (into {}
                         (map (fn [[id b]]
                                [id (use-def/block-uses-defs adapter b)]))
                         (:blocks cfg))
        ;; 初始状态
        init-li    (into {} (map (fn [[id info]] [id (:uses info)])) block-info)
        init-lo    (into {} (map (fn [[id _]] [id #{}])) block-info)]
    (loop [li       init-li
           lo       init-lo
           worklist order]
      (if (empty? worklist)
        {:live-in li :live-out lo}
        (let [b         (peek worklist)
              worklist  (pop worklist)
              ;; live-out[b] = ∪ live-in[succ]
              new-lo    (reduce (fn [acc s]
                                  (into acc (get li s #{})))
                                #{}
                                (get succs b []))
              ;; live-in[b] = use[b] ∪ (live-out[b] - def[b])
              {:keys [uses defs]} (get block-info b)
              new-li    (into uses (remove defs new-lo))
              changed?  (or (not= new-li (get li b))
                            (not= new-lo (get lo b)))]
          (if changed?
            (recur (assoc li b new-li)
                   (assoc lo b new-lo)
                   (into worklist (get preds b [])))
            (recur li lo worklist)))))))

;; ═══════════════════════════════════════════
;; 查询辅助
;; ═══════════════════════════════════════════

(defn live-in?
  "变量 var 在块 b 的入口是否活跃。"
  [liveness b var]
  (contains? (get-in liveness [:live-in b] #{}) var))

(defn live-out?
  "变量 var 在块 b 的出口是否活跃。"
  [liveness b var]
  (contains? (get-in liveness [:live-out b] #{}) var))

(defn live-in-set
  "块 b 的 live-in 集合。默认空集。"
  [liveness b]
  (get-in liveness [:live-in b] #{}))

(defn live-out-set
  "块 b 的 live-out 集合。默认空集。"
  [liveness b]
  (get-in liveness [:live-out b] #{}))

(defn var-live-blocks
  "变量 var 在哪些块的 live-in 里出现。返回块 id 集合。

   用于 pruned SSA 判断——变量 var 需要在哪些块的入口处理。
   实际插 phi 时配合支配边界使用：只在 DF 位置且 live-in 包含
   var 的块插。"
  [liveness var]
  (->> (:live-in liveness)
       (filter (fn [[_ vars]] (contains? vars var)))
       (map first)
       set))
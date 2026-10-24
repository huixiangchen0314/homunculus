(ns top.kzre.homunculus.core.cfg.analysis.liveness
  "后向活跃性分析。

   live-in[B]  = use[B] ∪ (live-out[B] - def[B])
   live-out[B] = ∪ live-in[succ]

   终止块无后继——live-out 是 ∅。

   ── worklist 算法 ────────────────────────────────────

   按逆后序初始化 worklist——后向数据流与逆后序方向一致。
   当某块的 live-in / live-out 变化时，把前驱加入 worklist。
   队列可以重复——不影响正确性，变化量单调递减。

   ── 初始状态 ─────────────────────────────────────────

   live-in[B] 初始化为 use[B]——下界（块内 use 不需要块外提供）。
   live-out[B] 初始化为 ∅。
   初始用 use 而非 ∅ 是收敛加速。

   ── 与 phi 的关系 ────────────────────────────────────

   本实现**在 phi 插入之前**计算——CFG 无 phi。
   用作 pruned SSA 判据：变量 x 在块 d 处需要 phi，当且仅当
   x ∈ live-in[d]。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.model :as cfg]))

;; ═══════════════════════════════════════════
;; 逆后序
;; ═══════════════════════════════════════════

(defn- rpo
  "从 entry 做 DFS，返回 reverse postorder。

   与 dominator.clj 的 dfs-postorder 独立——两者都需要此遍历，
   但计算目的不同。"
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
   adapter —— IAdapter 实例

   返回 {:live-in  {block-id → #{var...}}
         :live-out {block-id → #{var...}}}"
  [cfg adapter]
  (let [order      (rpo cfg)
        preds      (cfg/cfg-build-preds cfg)
        succs      (cfg/cfg-build-succs cfg)
        block-info (into {}
                         (map (fn [[id b]]
                                [id (adapt/block-uses-defs adapter b)]))
                         (:blocks cfg))
        init-li    (into {} (map (fn [[id info]] [id (:uses info)])) block-info)
        init-lo    (into {} (map (fn [[id _]] [id #{}])) block-info)]
    (loop [li       init-li
           lo       init-lo
           worklist order]
      (if (empty? worklist)
        {:live-in li :live-out lo}
        (let [b         (peek worklist)
              worklist  (pop worklist)
              new-lo    (reduce (fn [acc s]
                                  (into acc (get li s #{})))
                                #{}
                                (get succs b []))
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
  [liveness b var]
  (contains? (get-in liveness [:live-in b] #{}) var))

(defn live-out?
  [liveness b var]
  (contains? (get-in liveness [:live-out b] #{}) var))

(defn live-in-set
  [liveness b]
  (get-in liveness [:live-in b] #{}))

(defn live-out-set
  [liveness b]
  (get-in liveness [:live-out b] #{}))

(defn var-live-blocks
  [liveness var]
  (->> (:live-in liveness)
       (filter (fn [[_ vars]] (contains? vars var)))
       (map first)
       set))
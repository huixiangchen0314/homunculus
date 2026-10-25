(ns top.kzre.homunculus.core.cfg.analysis.liveness
  "后向活跃性分析。

   live-in[B]  = use[B] ∪ (live-out[B] - def[B])
   live-out[B] = ∪ live-in[succ]

   终止块无后继——live-out 是 ∅。

   ── 算法 ──

   按逆后序初始化待处理块——后向数据流与逆后序方向一致。
   当某块的 live-in / live-out 变化时，把前驱加入待处理块。
   队列可以重复——不影响正确性，变化量单调递减。

   ── 初始状态 ──

   live-in[B] 初始化为 use[B]——下界（块内 use 不需要块外提供）。
   live-out[B] 初始化为 ∅。
   初始用 use 而非 ∅ 是收敛加速。

   ── 与 phi 的关系 ──

   本实现**在 phi 插入之前**计算——CFG 无 phi。
   用作 pruned SSA 判据：变量 x 在块 d 处需要 phi，当且仅当
   x ∈ live-in[d]。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.model :as cfg])
  (:import (top.kzre.homunculus.core.cfg.model CFG)))

(defrecord Liveness [live-in live-out])

;; ═══════════════════════════════════════════
;; 逆后序
;; ═══════════════════════════════════════════

(defn- visit-postorder
  "DFS 后序访问。返回 [visited postorder]."
  [cfg block-id visited postorder]
  (if (contains? visited block-id)
    [visited postorder]
    (let [[visited' postorder']
          (reduce (fn [[visited-acc postorder-acc] successor-id]
                    (visit-postorder cfg successor-id
                                     visited-acc postorder-acc))
                  [(conj visited block-id) postorder]
                  (cfg/cfg-successors-of cfg block-id))]
      [visited' (conj postorder' block-id)])))

(defn- reverse-postorder
  "从 entry 做 DFS，返回 reverse postorder。

   与 dominator.clj 的 dfs-postorder 独立——两者都需要此遍历，
   但计算目的不同。"
  [^CFG cfg]
  (let [[_ postorder] (visit-postorder cfg (:entry cfg) #{} [])]
    (vec (reverse postorder))))

;; ═══════════════════════════════════════════
;; 主计算
;; ═══════════════════════════════════════════

(defn cfg-liveness
  "计算后向活跃性。

   cfg     —— CFG（普通，无 phi）
   adapter —— IAdapter 实例

   返回 {:live-in  {block-id → #{var-name ...}}
         :live-out {block-id → #{var-name ...}}}"
  [^CFG cfg adapter]
  (let [visit-order     (reverse-postorder cfg)
        predecessors    (cfg/cfg-build-predecessors cfg)
        successors      (cfg/cfg-build-successors cfg)
        block-uses-defs (into {}
                              (map (fn [[block-id block]]
                                     [block-id (adapt/block-uses-defs adapter block)]))
                              (:blocks cfg))
        initial-live-in  (into {} (map (fn [[block-id info]]
                                         [block-id (:uses info)]))
                               block-uses-defs)
        initial-live-out (into {} (map (fn [[block-id _]]
                                         [block-id #{}]))
                               block-uses-defs)]
    (loop [live-ins       initial-live-in
           live-outs      initial-live-out
           pending-blocks visit-order]
      (if (empty? pending-blocks)
        (->Liveness live-ins live-outs)
        (let [block-id     (peek pending-blocks)
              pending-blocks' (pop pending-blocks)
              new-live-out (reduce (fn [acc successor-id]
                                     (into acc (get live-ins successor-id #{})))
                                   #{}
                                   (get successors block-id []))
              {:keys [uses defs]} (get block-uses-defs block-id)
              new-live-in  (into uses (remove defs new-live-out))
              changed?     (or (not= new-live-in (get live-ins block-id))
                               (not= new-live-out (get live-outs block-id)))]
          (if changed?
            (recur (assoc live-ins block-id new-live-in)
                   (assoc live-outs block-id new-live-out)
                   (into pending-blocks' (get predecessors block-id [])))
            (recur live-ins live-outs pending-blocks')))))))

(defn stmts-liveness
  "逐语句级别的活跃分析。

   从最后一条语句往前：
   ▸ 最后一条语句的 live-out 是 ∅（块内局部分析，不跨块）
   ▸ 每条语句 live-in = use ∪ (live-out - def)
   ▸ 前一条语句的 live-out = 当前语句的 live-in

   返回与 stmts 等长的向量，第 i 个元素是 stmts[i] 执行前
   （:live-in）和执行后（:live-out）的活跃变量集。

   ── 注意 ──

   这是块内局部分析——初始 live-out 是 ∅。块出口的 term
   （:return 等）里的 use 不在此函数范围内。若 phi-var 在
   term 里被 use，phi-raise 的 collect-uses 已经能检测到
   term use 并跳过上浮，所以本函数只需要处理 stmts 内部的
   下游 use。"
  [stmts adapter]
  (let [[_ reversed-result]
        (reduce (fn [[live-out acc] stmt]
                  (let [uses    (adapt/stmt-uses adapter stmt)
                        defs    (adapt/stmt-defs adapter stmt)
                        live-in (into uses (remove defs live-out))]
                    [live-in (conj acc (->Liveness live-in live-out))]))
                [#{} []]
                (reverse stmts))]
    (vec (reverse reversed-result))))

;; ═══════════════════════════════════════════
;; 查询辅助
;; ═══════════════════════════════════════════

(defn live-in?
  [^Liveness liveness block-id var-name]
  (contains? (get-in liveness [:live-in block-id] #{}) var-name))

(defn live-out?
  [^Liveness liveness block-id var-name]
  (contains? (get-in liveness [:live-out block-id] #{}) var-name))

(defn live-in-set
  [^Liveness liveness block-id]
  (get-in liveness [:live-in block-id] #{}))

(defn live-out-set
  [^Liveness liveness block-id]
  (get-in liveness [:live-out block-id] #{}))

(defn var-live-blocks
  [^Liveness liveness var-name]
  (->> (:live-in liveness)
       (filter (fn [[_ vars]] (contains? vars var-name)))
       (map first)
       set))


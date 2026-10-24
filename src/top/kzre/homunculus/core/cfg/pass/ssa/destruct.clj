(ns top.kzre.homunculus.core.cfg.pass.ssa.destruct
  "Phi 消除——SSA-CFG → 普通 CFG。

   ── 两种方法 ────────────────────────────────────────

   方法 1（前驱直连 return）：
     块 B 的 term 是 :return value，且 value 直接引用 phi var，
     且 B 的 stmts 为空，且 B 的所有前驱的 term 是 :jump B——
     各前驱的 term 改成 :return phi-input，删除 B。
     收益：B 消失，phi var 消失，直接 return 实际值。

   方法 2（并行复制，fallback）：
     各前驱末尾插入 phi-var = phi-input 的 move，保留 B。
     用于方法 1 不满足条件的 phi（循环头、有 stmts 的合并块等）。

   ── 迭代到不动点 ────────────────────────────────────

   方法 1 的消除可能导致新的块满足方法 1 条件——比如 B 的前驱
   P 原本 term 是 :jump B，删除 B 后 P 的 term 变成 :return，
   如果 P 自己也有 phi 且 stmts 空，P 现在也满足方法 1。

   迭代到没有新的块满足方法 1 为止，然后剩下的 phi 走方法 2。

   ── 方法 1 的简化条件 ──────────────────────────────

   只处理\"B 的 return 直接是单个 phi var\"的情况。
   `return (+ x$$1 y$$1)` 这类复合表达式不走方法 1——需要把
   整个 return 表达式下推到每个前驱并替换所有 phi var，复杂
   且收益低。走方法 2 保持保守。"
  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.protocol :as proto]
    [top.kzre.homunculus.core.cfg.phi :as phi]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 方法 2：并行复制
;; ═══════════════════════════════════════════

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

;; ═══════════════════════════════════════════
;; 方法 1：前驱直连 return
;; ═══════════════════════════════════════════

(defn- eligible-for-return-elimination?
  "块 B 是否满足方法 1 的条件：

   ▸ B 的 stmts 为空
   ▸ B 的 term 是 :return value
   ▸ value 是 :variable
   ▸ value 的 name 是 B 某个 phi 的 var
   ▸ B 的所有前驱的 term 是 :jump B"
  [ssa-cfg id]
  (let [block (cfg/cfg-block ssa-cfg id)
        phis  (:phis block)
        term  (:term block)
        stmts (:stmts block)
        preds (get (cfg/cfg-build-preds ssa-cfg) id [])]
    (and (empty? stmts)
         (seq phis)
         (= :return (proto/term-kind term))
         (let [v (:value term)]
           (and v (= :variable (ast/kind v))))
         (let [v-name (:name (:value term))]
           (some #(= v-name (phi/phi-var %)) phis))
         (every? #(= :jump (proto/term-kind (:term (cfg/cfg-block ssa-cfg %))))
                 preds))))

(defn- eliminate-via-return
  "方法 1：各前驱的 term 改成 :return phi-input，删除 B。"
  [ssa-cfg id]
  (let [block      (cfg/cfg-block ssa-cfg id)
        phis       (:phis block)
        term       (:term block)
        v-name     (:name (:value term))
        target-phi (some #(when (= v-name (phi/phi-var %)) %) phis)
        preds      (get (cfg/cfg-build-preds ssa-cfg) id [])]
    (reduce
      (fn [cfg' pred]
        (let [input    (some (fn [{input-pred :pred :keys [value]}]   ; ← 重命名
                               (when (= pred input-pred) value))       ; ← pred 是外层参数
                             (phi/phi-inputs target-phi))
              new-term (cfg/->Return input)]
          (cfg/cfg-replace-term cfg' pred new-term)))
      (cfg/cfg-remove-block ssa-cfg id)
      preds)))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn- eliminate-method-1-fixpoint
  "迭代走方法 1，直到没有新的块满足条件。

   每次迭代至少删除一个块（方法 1 必然删除被消除的块）——
   最多迭代 (count blocks) 次，不会无限循环。"
  [ssa-cfg]
  (loop [c ssa-cfg]
    (let [candidates (filterv #(eligible-for-return-elimination? c %)
                              (keys (:blocks c)))]
      (if (empty? candidates)
        c
        (recur (reduce eliminate-via-return c candidates))))))

(defn elim-phi
  "消除所有 phi 节点。

   1. 迭代走方法 1——能直连 return 的 phi 全部消除
   2. 剩余 phi 走方法 2——各前驱插入 move

   返回普通 CFG——:phis 置 nil（不是 dissoc——dissoc record 字段
   会降级为普通 map，破坏 bb-* 协议）。"
  [ssa-cfg make-move-fn]
  (let [after-method-1  (eliminate-method-1-fixpoint ssa-cfg)
        after-method-2  (reduce
                          (fn [c id]
                            (if (:phis (cfg/cfg-block c id))
                              (eliminate-phi-block c id make-move-fn)
                              c))
                          after-method-1
                          (keys (:blocks after-method-1)))]
    (update after-method-2 :blocks
            (fn [blocks]
              (into {}
                    (map (fn [[id b]] [id (assoc b :phis nil)]))
                    blocks)))))
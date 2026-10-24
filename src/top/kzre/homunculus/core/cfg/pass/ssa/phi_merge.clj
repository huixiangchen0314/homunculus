(ns top.kzre.homunculus.core.cfg.pass.ssa.phi-merge
  "phi-var 合并——消除 `phi_var = phi[...]; x = phi_var` 的中间变量。

   ── 目标 ──

   把内层 if 的 phi-var 合并到外层——13 层 ifval86N 全部合并到
   ifval8649，只剩一个 phi-var。

   ── 识别条件 ──

   ▸ 块 M 恰好一个 phi——phi-var
   ▸ 块 M 恰好一个 stmt——simple move 形态：`x = rhs`，且 rhs
     是 phi-var
   ▸ phi-var 在 M 之外没有 stmt/term use

   ── 变换 ──

   1. 所有块（含前驱、后继、外层 phi 所在块）的 stmts/term/
      phi 输入里 phi_var → x_sym
   2. M 的 phi-var 改名为 x_sym——phi 直接定义 x_sym
   3. M 的 stmt `x_sym = phi_var` 替换后变成 `x_sym = x_sym`——
      **不动**，留给 lower 的 strip-self-assign 清理

   ── 为什么检查 phi 输入会阻止合并 ──

   phi-var 作为外层 phi 的输入是合法的 SSA 链（内层 phi 输出
   → 外层 phi 输入）。如果把它算作 \"elsewhere use\"，采样链
   的每一层都会被阻止——合并永远不会发生。合并时所有块的
   phi 输入会一并替换成 x_sym，所以不需要阻止。

   ── 为什么只替换前驱不够 ──

   外层 phi 在 M 的后继块里（BB_merge0 是 BB_merge1 的后继）。
   只替换前驱的话，外层 phi 的输入仍引用已改名的旧 phi-var——
   引用失效。必须遍历所有块替换。

   ── 职责边界 ──

   ▸ phi-merge 只做 phi-var 改名和 use 替换——不碰 IRStmt 形态
   ▸ 自赋值清理是 lower（varversion）的职责
   ▸ return 中转消除（方法 1）不用——违背\"函数单一出口\"语义

   ── 依赖 ──

   只用 cfg.model / cfg.phi / cfg.adapt 协议——不依赖 irstmt.ast。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.phi :as phi]))

;; ═══════════════════════════════════════════
;; 识别
;; ═══════════════════════════════════════════

(defn- single-move-block?
  "块 M 是否是单 phi + 单 simple-move，且 move 的 rhs 就是 phi-var。"
  [block adapter]
  (let [stmts (:stmts block)
        phis  (:phis block)]
    (when (and (= 1 (count phis))
               (= 1 (count stmts)))
      (let [phi-var          (phi/phi-var (first phis))
            [x-sym rhs-node] (adapt/simple-move? adapter (first stmts))]
        (when (and x-sym
                   rhs-node
                   (= #{phi-var} (adapt/expr-uses adapter rhs-node)))
          {:x-sym x-sym :phi-var phi-var})))))

(defn- phi-var-used-elsewhere?
  "phi_var 在 merge 块之外是否有 stmt/term use。

   不检查 phi 输入——phi-var 作为其他 phi 的输入是合法的 SSA 链
   （内层 phi 输出 → 外层 phi 输入），合并时所有块的 phi 输入
   会一并替换成 x-sym，不需要阻止。"
  [ssa-cfg m-id phi-var adapter]
  (boolean
    (some (fn [[bid block]]
            (when (not= bid m-id)
              (or (some (fn [stmt]
                          (contains? (adapt/stmt-uses adapter stmt) phi-var))
                        (:stmts block))
                  (contains? (adapt/term-uses adapter (:term block)) phi-var))))
          (:blocks ssa-cfg))))

;; ═══════════════════════════════════════════
;; 变换
;; ═══════════════════════════════════════════

(defn- replace-var-in-block
  "在 block 的 stmts（uses + defs）/ term / phi 输入里把 old 替换成 new。
   phi 的 var 不改——由调用方处理。"
  [block old new adapter]
  (-> block
      (update :stmts
              (fn [stmts]
                (mapv (fn [stmt]
                        (-> stmt
                            (adapt/stmt-replace-uses adapter {old new})
                            (adapt/stmt-replace-defs adapter {old new})))
                      stmts)))
      (update :term
              (fn [term]
                (adapt/term-replace-uses adapter term {old new})))
      (update :phis
              (fn [phis]
                (when phis
                  (mapv (fn [p]
                          (update p :inputs
                                  (fn [inputs]
                                    (mapv (fn [{:keys [value] :as input}]
                                            (if value
                                              (assoc input :value
                                                           (adapt/expr-replace-uses
                                                             adapter value {old new}))
                                              input))
                                          inputs))))
                        phis))))))

(defn- phi-merge-block
  "对块 m-id 尝试 phi-merge。不适用时返回原 cfg。

   ▸ merge 块 M：所有 stmts/term/phi 输入替换 phi_var → x_sym，
     然后 phi var 改名 phi_var → x_sym（phi 直接定义 x_sym）
   ▸ 其他块：所有 stmts/term/phi 输入替换 phi_var → x_sym
     （外层 phi 的输入在这里被替换——不是只替换前驱）"
  [ssa-cfg m-id adapter]
  (let [block (get (:blocks ssa-cfg) m-id)
        info  (single-move-block? block adapter)]
    (if (and info
             (not (phi-var-used-elsewhere? ssa-cfg m-id (:phi-var info) adapter)))
      (let [{:keys [x-sym phi-var]} info]
        (update ssa-cfg :blocks
                (fn [blocks]
                  (into {}
                        (map (fn [[bid b]]
                               [bid
                                (let [b' (replace-var-in-block b phi-var x-sym adapter)]
                                  (if (= bid m-id)
                                    ;; merge 块：phi var 改名
                                    (update b' :phis
                                            (fn [phis]
                                              (mapv #(phi/rename-phi-var % x-sym) phis)))
                                    b'))]))
                        blocks)))))
    ssa-cfg))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn- phi-var-set
  "当前 cfg 里所有 phi 的 var 集合——用于收敛判断。

   合并只改 phi-var 的名字，phi 数量不变——用名字集合变化
   判断是否还有合并发生。"
  [cfg]
  (into #{}
        (mapcat (fn [[_ b]] (map phi/phi-var (:phis b))))
        (:blocks cfg)))

(defn phi-merge
  "迭代 phi-merge 到不动点。

   每轮 reduce 处理所有块——合并一个块可能让外层块变得可合并。
   收敛判断用 phi-var 名字集合——集合变化说明本轮有合并发生。"
  [ssa-cfg adapter]
  (loop [cfg ssa-cfg n 0]
    (when (> n 1000)
      (throw (ex-info "phi-merge did not converge" {:iterations n})))
    (let [before (phi-var-set cfg)
          cfg'   (reduce (fn [c id] (phi-merge-block c id adapter))
                         cfg
                         (keys (:blocks cfg)))]
      (if (= before (phi-var-set cfg'))
        cfg'
        (recur cfg' (inc n))))))
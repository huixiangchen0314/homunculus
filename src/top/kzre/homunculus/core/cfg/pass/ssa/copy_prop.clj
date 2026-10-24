(ns top.kzre.homunculus.core.cfg.pass.ssa.copy-prop
  "SSA copy propagation——消除 'x = y' 形式的复制赋值。

   ── 目标 ────────────────────────────────────────────

   在 SSA-CFG（destruct 之前）上消除：
   ▸ x、y 都是 :variable 节点
   ▸ x 不是 phi 输出（phi 输出是多路径值的合并，不能替换成单一源头）
   ▸ y 不是 phi 输出（同——替换 phi 输出会引入跨路径错误）

   ── 算法 ────────────────────────────────────────────

   1. 找所有满足条件的 'x = y'
   2. 全 CFG 替换所有 use(x) → use(y)——包括 stmts、terms、phi 输入
   3. 删除 'x = y' 本身
   4. 迭代——替换可能产生新的 copy 链（x = y; z = x → z = y），
      下一轮消除

   每轮至少删除一个 stmt，必然收敛。

   ── 为什么跳过 phi 输出 ────────────────────────────

   phi 输出 x$$1 = phi [p1: v1, p2: v2]——不同路径上值不同。
   把它替换成 v1 或 v2，另一条路径就错。copy-prop 只处理单一
   定义的普通 assign；phi 输出走 phi 消除（方法 1 或方法 2）。

   ── phi 输入替换 ────────────────────────────────────

   copy-prop 后 phi 的输入也需要替换——phi [p1: x] 中 x = y 后
   x 就是 y，phi [p1: y] 等价。这一步不破坏 phi 的路径语义——
   phi 的 :pred 还是 p1，只是 :value 换成\"路径上实际等价的变量\"。"
  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.phi :as phi]
    [top.kzre.homunculus.core.cfg.analysis.use-def :as use-def]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 候选识别
;; ═══════════════════════════════════════════

(defn- phi-vars
  "所有 phi 输出的变量名集合。"
  [ssa-cfg]
  (into #{}
        (map phi/phi-var)
        (mapcat :phis (vals (:blocks ssa-cfg)))))

(defn- copy-assign?
  "stmt 是否是 'x = y' 形式——x、y 都是 :variable，且都不在 phi-vars。"
  [stmt phi-var-set]
  (and (= :assign (ast/kind stmt))
       (let [v (:var stmt)
             r (:val stmt)]
         (and v r
              (= :variable (ast/kind v))
              (= :variable (ast/kind r))
              (not (contains? phi-var-set (:name v)))
              (not (contains? phi-var-set (:name r)))))))

(defn- collect-copy-assigns
  "找所有满足条件的 'x = y'，按 block-id + 反向 idx 排序。

   反向 idx：同一块内从后往前删——前面的 idx 不受影响。
   跨块互不影响（不同 block 的 stmts 长度独立）。"
  [ssa-cfg phi-var-set]
  (->> (for [[bid block] (:blocks ssa-cfg)
             [idx stmt]  (map-indexed vector (:stmts block))
             :when       (copy-assign? stmt phi-var-set)]
         {:block-id bid
          :stmt-idx idx
          :x        (:name (:var stmt))
          :y        (:name (:val stmt))})
       (sort-by (juxt :block-id (comp - :stmt-idx)))
       vec))

;; ═══════════════════════════════════════════
;; 替换
;; ═══════════════════════════════════════════

(defn- replace-in-stmts
  [stmts x y adapter]
  (mapv #(use-def/stmt-replace-uses adapter % {x y}) stmts))

(defn- replace-in-term
  [term x y adapter]
  (use-def/term-replace-uses adapter term {x y}))

(defn- replace-in-phi
  "替换 phi 输入里引用 x 的 :value 为 y。:pred 不变。"
  [p x y adapter]
  (let [ref-y (use-def/make-variable-ref adapter y)]
    (update p :inputs
            (fn [inputs]
              (mapv (fn [{:keys [value] :as input}]
                      (if (and value
                               (= :variable (ast/kind value))
                               (= x (:name value)))
                        (assoc input :value ref-y)
                        input))
                    inputs)))))

(defn- replace-all-uses
  "全 CFG 替换 use(x) → use(y)——stmts、terms、phi 输入。"
  [ssa-cfg x y adapter]
  (update ssa-cfg :blocks
          (fn [blocks]
            (into {}
                  (map (fn [[bid block]]
                         [bid (-> block
                                  (update :stmts #(replace-in-stmts % x y adapter))
                                  (update :term  #(replace-in-term  % x y adapter))
                                  (update :phis  (fn [phis]
                                                   (when phis
                                                     (mapv (fn [p]
                                                             (replace-in-phi p x y adapter))
                                                           phis)))))])
                       blocks)))))

(defn- remove-copy-stmt
  "从块里删除指定 idx 的 stmt。"
  [ssa-cfg bid idx]
  (update-in ssa-cfg [:blocks bid :stmts]
             (fn [stmts]
               (vec (concat (take idx stmts) (drop (inc idx) stmts))))))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn copy-prop
  "SSA copy propagation——迭代到不动点。"
  [ssa-cfg adapter]
  (loop [cfg ssa-cfg
         n   0]
    (when (> n 1000)
      (throw (ex-info "copy-prop did not converge"
                      {:iterations n
                       :block-count (count (:blocks cfg))})))
    (let [pvars  (phi-vars cfg)
          copies (collect-copy-assigns cfg pvars)]
      (if (empty? copies)
        cfg
        (let [cfg' (reduce (fn [c {:keys [block-id stmt-idx x y]}]
                             (-> c
                                 (replace-all-uses x y adapter)
                                 (remove-copy-stmt block-id stmt-idx)))
                           cfg
                           copies)]
          (recur cfg' (inc n)))))))
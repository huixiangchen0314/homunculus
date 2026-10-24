(ns top.kzre.homunculus.core.cfg.irstmt.pass.copy-prop
  "SSA copy propagation——消除 'x = y' 形式的复制赋值。

   ── 为什么在 irstmt 包下 ──

   copy-assign? 直接判断 IRStmt 的 :assign 节点结构（ast/kind、
   :var/:val 字段、:variable 节点），不是协议无关的 CFG 变换。
   放在 irstmt/pass/ 下表明它是 IRStmt 适配层的实现。

   ── 目标 ──

   ▸ x、y 都是 :variable
   ▸ x 不是 phi 输出（phi 输出多路径合并，不能替换）
   ▸ y 不是 phi 输出（同——替换 phi 输出引入跨路径错误）

   ── 算法 ──

   1. 找所有满足条件的 'x = y'
   2. 全 CFG 替换 use(x) → use(y)——stmts、terms、phi 输入
   3. 删除 'x = y' 本身
   4. 迭代到不动点——替换可能产生新 copy 链

   ── 与 phi-raise 的关系 ──

   phi-raise 在 destruct 之后把 phi-var 的 move 上浮到 def——
   phi-var 的 move 消失。copy-prop 运行时 phi-var-set 通常为
   空，phi-var 跳过条件不生效。保留该条件是为了 phi-raise 未
   覆盖的场景（如多个 move 目标不一致时 phi-var 残留）。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.phi :as phi]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 候选识别
;; ═══════════════════════════════════════════

(defn- phi-vars
  [ssa-cfg]
  (into #{}
        (map phi/phi-var)
        (mapcat :phis (vals (:blocks ssa-cfg)))))

(defn- copy-assign?
  [stmt phi-var-set]
  (and (= :assign (ast/kind stmt))
       (let [v (:var stmt) r (:val stmt)]
         (and v r
              (= :variable (ast/kind v))
              (= :variable (ast/kind r))
              (not (contains? phi-var-set (:name v)))
              (not (contains? phi-var-set (:name r)))))))

(defn- collect-copy-assigns
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
  (mapv #(adapt/stmt-replace-uses adapter % {x y}) stmts))

(defn- replace-in-term
  [term x y adapter]
  (adapt/term-replace-uses adapter term {x y}))

(defn- replace-in-phi
  [p x y adapter]
  (let [ref-y (adapt/make-var-ref adapter (phi/phi-template p) y)]
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
  [ssa-cfg bid idx]
  (update-in ssa-cfg [:blocks bid :stmts]
             (fn [stmts]
               (vec (concat (take idx stmts) (drop (inc idx) stmts))))))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn copy-prop
  [ssa-cfg adapter]
  (loop [cfg ssa-cfg n 0]
    (when (> n 1000)
      (throw (ex-info "copy-prop did not converge" {:iterations n})))
    (let [pvars  (phi-vars cfg)
          copies (collect-copy-assigns cfg pvars)]
      (if (empty? copies)
        cfg
        (let [cfg' (reduce (fn [c {:keys [block-id stmt-idx x y]}]
                             (-> c
                                 (replace-all-uses x y adapter)
                                 (remove-copy-stmt block-id stmt-idx)))
                           cfg copies)]
          (recur cfg' (inc n)))))))
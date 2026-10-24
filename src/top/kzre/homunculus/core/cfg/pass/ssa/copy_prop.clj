(ns top.kzre.homunculus.core.cfg.pass.ssa.copy-prop
  "SSA copy propagation——消除 'x = y' 形式的复制赋值。

   ── 目标 ──

   ▸ x、y 都是 :variable
   ▸ x 不是 phi 输出（phi 输出多路径合并，不能替换）
   ▸ y 不是 phi 输出（同——替换 phi 输出引入跨路径错误）

   ── 算法 ──

   1. 找所有满足条件的 'x = y'
   2. 全 CFG 替换 use(x) → use(y)——stmts、terms、phi 输入
   3. 删除 'x = y' 本身
   4. 迭代到不动点——替换可能产生新 copy 链

   ── phi 输入的替换 ──

   phi [p1: x] 且 x = y——替换成 phi [p1: y] 等价。
   通过 adapt/make-var-ref 从 phi 的 template 继承 attrs。"
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
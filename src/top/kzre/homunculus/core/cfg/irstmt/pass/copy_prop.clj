(ns top.kzre.homunculus.core.cfg.irstmt.pass.copy-prop
  "SSA copy propagation——消除 'x = y' 形式的复制赋值。

   ── 为什么在 irstmt 包下 ──

   copy-assign? 直接判断 IRStmt 的 :assign 节点结构（ast/kind、
   :var/:val 字段、:variable 节点），不是协议无关的 CFG 变换。
   放在 irstmt/pass/ 下表明它是 IRStmt 适配层的实现。

   ── 目标 ──

   ▸ x、y 都是 :variable
   ▸ x、y 不在 protected-names 里——protected-names 是 phi
     input 的 var-name 集合

   ── 为什么只保护 phi input ──

   替换 phi input var-name 的 use 会破坏 phi 的 input——phi 的
   语义直接依赖 input 的 var-name。phi 的 var-name 不需要保护
   ——SSA 下 phi 的 var-name 在 join 处定义一次，之后不重新
   定义——`x = phi_var` 的传播（替换 x 的 use → phi_var）是
   安全的。

   protected-names 里是精确版本名（`a$$1`）——直接 contains?
   匹配，不需要 strip-versions。

   ── 算法 ──

   1. 找所有满足条件的 'x = y'
   2. 全 CFG 替换 use(x) → use(y)——stmts、terms
   3. 删除 'x = y' 本身
   4. 迭代到不动点

   phi 的 input 不替换——protected-names 保护了 phi input 的
   var-name，x/y 不会是它们。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.phi :as phi]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 受保护名称——phi input 的 var-name
;; ═══════════════════════════════════════════

(defn- protected-names
  "copy-prop 不能碰的变量名——phi input 的 var-name 集合。

   这些是 phi 语义的直接依赖——替换它们的 use 会破坏 phi 的
   input。精确版本名，直接 contains? 匹配。

   phi 的 var-name 不在此列——SSA 下 phi 的 var-name 在 join
   处定义一次，之后不重新定义——替换它的 use 是安全的。"
  [ssa-cfg]
  (into #{}
        (mapcat (fn [[_ block]]
                  (mapcat (fn [phi]
                            (keep :var-name (phi/phi-inputs phi)))
                          (:phis block))))
        (:blocks ssa-cfg)))

;; ═══════════════════════════════════════════
;; 候选识别
;; ═══════════════════════════════════════════

(defn- copy-assign?
  [stmt protected]
  (and (= :assign (ast/kind stmt))
       (let [v (:var stmt) r (:val stmt)]
         (and v r
              (= :variable (ast/kind v))
              (= :variable (ast/kind r))
              (not (contains? protected (:name v)))
              (not (contains? protected (:name r)))))))

(defn- collect-copy-assigns
  [ssa-cfg protected]
  (->> (for [[bid block] (:blocks ssa-cfg)
             [idx stmt]  (map-indexed vector (:stmts block))
             :when       (copy-assign? stmt protected)]
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

(defn- replace-all-uses
  "替换 stmts 和 term 里的 use(x) → use(y)。

   不替换 phi 的 input——protected-names 保护了 phi input 的
   var-name，x/y 不会是它们；phi 的 input 的 origin 的 name
   也是版本名——不会等于普通变量 x/y。"
  [ssa-cfg x y adapter]
  (update ssa-cfg :blocks
          (fn [blocks]
            (into {}
                  (map (fn [[bid block]]
                         [bid (-> block
                                  (update :stmts #(replace-in-stmts % x y adapter))
                                  (update :term  #(replace-in-term  % x y adapter)))]))
                  blocks))))

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
    (let [protected (protected-names cfg)
          copies    (collect-copy-assigns cfg protected)]
      (if (empty? copies)
        cfg
        (let [cfg' (reduce (fn [c {:keys [block-id stmt-idx x y]}]
                             (-> c
                                 (replace-all-uses x y adapter)
                                 (remove-copy-stmt block-id stmt-idx)))
                           cfg copies)]
          (recur cfg' (inc n)))))))
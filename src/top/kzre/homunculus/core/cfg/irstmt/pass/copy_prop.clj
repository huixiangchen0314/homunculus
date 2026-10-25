(ns top.kzre.homunculus.core.cfg.irstmt.pass.copy-prop
  "SSA copy propagation——消除 'x = y' 形式的复制赋值。

   ── 为什么在 irstmt 包下 ──

   copy-assign? 直接判断 IRStmt 的 :assign 节点结构（ast/kind、
   :var/:val 字段、:variable 节点），不是协议无关的 CFG 变换。
   放在 irstmt/pass/ 下表明它是 IRStmt 适配层的实现。

   ── 目标 ──

   ▸ x、y 都是 :variable
   ▸ x 的 base name 不是 phi-var-name（phi 变量不能替换）
   ▸ y 的 base name 不是 phi-var-name

   SSA 环境下 x/y 带版本号（`a$$1`）——phi-var-name-set 里是
   base name（`a`）。用 strip-versions 后比较。

   ── 算法 ──

   1. 找所有满足条件的 'x = y'
   2. 全 CFG 替换 use(x) → use(y)——stmts、terms、phi 输入
   3. 删除 'x = y' 本身
   4. 迭代到不动点

   ── 与 raise-phi 的关系 ──

   raise-phi 在 construct 之后、destruct 之前——phi 还在 :phis
   里，phi-var-name 是 base name。copy-prop 在 raise-phi 之后
   跑——phi-var-name-set 非空，跳过 phi 涉及的变量。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.phi :as phi]
    [top.kzre.homunculus.core.cfg.pass.ssa.construct :as construct]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 候选识别
;; ═══════════════════════════════════════════

(defn- phi-var-names
  "当前 CFG 里所有 phi 的 var-name 集合（base name）。"
  [ssa-cfg]
  (into #{}
        (map phi/phi-var-name)
        (mapcat :phis (vals (:blocks ssa-cfg)))))

(defn- copy-assign?
  "x = y 形式的拷贝赋值。

   x/y 的 base name（strip 版本号）不在 phi-var-name-set 里——
   跳过 phi 涉及的变量。"
  [stmt phi-var-name-set]
  (and (= :assign (ast/kind stmt))
       (let [v (:var stmt) r (:val stmt)]
         (and v r
              (= :variable (ast/kind v))
              (= :variable (ast/kind r))
              (not (contains? phi-var-name-set
                              (construct/strip-versions (:name v))))
              (not (contains? phi-var-name-set
                              (construct/strip-versions (:name r))))))))

(defn- collect-copy-assigns
  [ssa-cfg phi-var-name-set]
  (->> (for [[bid block] (:blocks ssa-cfg)
             [idx stmt]  (map-indexed vector (:stmts block))
             :when       (copy-assign? stmt phi-var-name-set)]
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
  "替换 phi input 的 origin 和 var-name。

   防御性代码——copy-assign? 跳过 phi 涉及的变量，phi input
   的 origin/var-name 的 base name 在 phi-var-name-set 里——
   实际不会触发。保留以应对未来放松跳过条件。"
  [p x y adapter]
  (let [ref-y (adapt/make-var-ref adapter (phi/phi-origin p) y)]
    (update p :inputs
            (fn [inputs]
              (mapv (fn [input]
                      (if (and (:origin input)
                               (= :variable (ast/kind (:origin input)))
                               (= x (:name (:origin input))))
                        (assoc input :origin ref-y :var-name y)
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
    (let [phi-var-name-set (phi-var-names cfg)
          copies           (collect-copy-assigns cfg phi-var-name-set)]
      (if (empty? copies)
        cfg
        (let [cfg' (reduce (fn [c {:keys [block-id stmt-idx x y]}]
                             (-> c
                                 (replace-all-uses x y adapter)
                                 (remove-copy-stmt block-id stmt-idx)))
                           cfg copies)]
          (recur cfg' (inc n)))))))
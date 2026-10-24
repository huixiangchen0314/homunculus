(ns top.kzre.homunculus.core.cfg.pass.ssa.raise-phi
  "phi 提升。

   ── 结构（以 ifval8779 为例）──

     head:   branch c → then, else
     then:   ifval8779 = texture12
     else:   ifval8779 = vec4(0)
     after:  ifval8778 = ifval8779       ← 捕获

   ── 流程 ──

   1. 扫描全 CFG，找所有捕获 `target = phi_var`（simple-move，
      uses 恰好一个 phi-var）
   2. 按 phi-var 分组，过滤掉被捕获多次的（提升价值不大）
   3. 对每个单捕获，检查：
      ▸ phi-var 有 def，def 块有共同后继 after
      ▸ 捕获块不是 def 块（跨路径）
      ▸ target 的其他 def 不在 def 块 / after 块（改名不冲突）
      ▸ 三步判断（捕获之后无 use / 捕获是 simple-move / 无副作用）
   4. 变换：phi-var 的 def lhs → target，所有 use → target

   ── 依赖 ──

   只用 cfg.adapt / cfg.model——不依赖 irstmt.ast。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [clojure.set :as set]))

;; ═══════════════════════════════════════════
;; 收集
;; ═══════════════════════════════════════════

(defn- phi-var-set
  [ssa-cfg]
  (into #{} (map :var) (mapcat :phis (vals (:blocks ssa-cfg)))))

(defn- find-captures
  "扫描全 CFG，找所有捕获 `target = phi_var`。

   simple-move 且 uses 恰好一个变量，且该变量是 phi-var。
   返回 [{:block-id :stmt :phi-var :target}]。"
  [ssa-cfg adapter]
  (let [pvars (phi-var-set ssa-cfg)]
    (vec
      (for [[bid block] (:blocks ssa-cfg)
            stmt (:stmts block)
            :when (adapt/simple-move? adapter stmt)
            :let [target (first (adapt/stmt-defs adapter stmt))
                  uses   (adapt/stmt-uses adapter stmt)]
            :when (and target (= 1 (count uses)))
            :let [phi-var (first uses)]
            :when (contains? pvars phi-var)]
        {:block-id bid
         :stmt     stmt
         :phi-var  phi-var
         :target   target}))))

(defn- def-blocks
  [ssa-cfg phi-var adapter]
  (into #{}
        (for [[bid block] (:blocks ssa-cfg)
              stmt (:stmts block)
              :when (contains? (adapt/stmt-defs adapter stmt) phi-var)]
          bid)))

(defn- other-uses
  "phi-var 在 capture-stmt 之外的所有 use。"
  [ssa-cfg phi-var capture-stmt adapter]
  (vec
    (concat
      (for [[_ block] (:blocks ssa-cfg)
            stmt (:stmts block)
            :when (and (not (identical? stmt capture-stmt))
                       (contains? (adapt/stmt-uses adapter stmt) phi-var))]
        {:kind :stmt :stmt stmt})
      (for [[_ block] (:blocks ssa-cfg)
            :when (contains? (adapt/term-uses adapter (:term block)) phi-var)]
        {:kind :term :term (:term block)}))))

(defn- after-block
  "def 块的共同后继。不存在时返回 nil。"
  [ssa-cfg dbs]
  (when (seq dbs)
    (let [succ-sets (map (fn [bid]
                           (set (cfg/cfg-successors-of ssa-cfg bid)))
                         dbs)]
      (when (every? seq succ-sets)
        (first (apply set/intersection succ-sets))))))

(defn- target-def-in-branch-or-after?
  [ssa-cfg target capture-stmt dbs ab adapter]
  (boolean
    (some (fn [[bid block]]
            (when (or (contains? dbs bid) (= bid ab))
              (some (fn [stmt]
                      (and (not (identical? stmt capture-stmt))
                           (contains? (adapt/stmt-defs adapter stmt) target)))
                    (:stmts block))))
          (:blocks ssa-cfg))))

;; ═══════════════════════════════════════════
;; 三步判断
;; ═══════════════════════════════════════════

(defn- use-side-effect?
  [{:keys [kind stmt]} adapter]
  (case kind
    :stmt (adapt/stmt-fx? adapter stmt)
    :term false))

(defn- can-raise-capture?
  "三步判断。返回布尔。"
  [ssa-cfg phi-var capture-stmt adapter]
  (let [others (other-uses ssa-cfg phi-var capture-stmt adapter)]
    (or (empty? others)
        (adapt/simple-move? adapter capture-stmt)
        (and (not (adapt/stmt-fx? adapter capture-stmt))
             (not-any? #(use-side-effect? % adapter) others)))))

;; ═══════════════════════════════════════════
;; 变换
;; ═══════════════════════════════════════════

(defn- raise-defs
  [ssa-cfg phi-var target adapter]
  (update ssa-cfg :blocks
          (fn [blocks]
            (into {}
                  (map (fn [[bid b]]
                         [bid (update b :stmts
                                      (fn [ss]
                                        (mapv #(adapt/stmt-replace-defs
                                                 adapter % {phi-var target})
                                              ss)))]))
                  blocks))))

(defn- replace-uses
  [ssa-cfg phi-var target adapter]
  (update ssa-cfg :blocks
          (fn [blocks]
            (into {}
                  (map (fn [[bid b]]
                         [bid (-> b
                                  (update :stmts
                                          (fn [ss]
                                            (mapv #(adapt/stmt-replace-uses
                                                     adapter % {phi-var target})
                                                  ss)))
                                  (update :term
                                          #(adapt/term-replace-uses
                                             adapter % {phi-var target})))]))
                  blocks))))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn- candidate-for-capture
  "对单个捕获检查所有条件。返回 [phi-var target] 或 nil。"
  [ssa-cfg {:keys [block-id stmt phi-var target]} adapter]
  (let [dbs (def-blocks ssa-cfg phi-var adapter)
        ab  (after-block ssa-cfg dbs)]
    (when (and (seq dbs)
               ab
               (not (contains? dbs block-id))
               (not (target-def-in-branch-or-after?
                      ssa-cfg target stmt dbs ab adapter))
               (can-raise-capture? ssa-cfg phi-var stmt adapter))
      [phi-var target])))

(defn- find-candidate
  "找所有捕获，按 phi-var 分组，过滤多捕获，返回第一个
   通过全部检查的 [phi-var target]。"
  [ssa-cfg adapter]
  (let [captures (find-captures ssa-cfg adapter)
        by-phi   (group-by :phi-var captures)]
    (some (fn [[_ cs]]
            (when (= 1 (count cs))
              (candidate-for-capture ssa-cfg (first cs) adapter)))
          by-phi)))

(defn raise-phi
  "迭代提升到不动点。"
  [ssa-cfg adapter]
  (loop [cfg ssa-cfg n 0]
    (when (> n 10000)
      (throw (ex-info "raise-phi did not converge" {:iterations n})))
    (if-let [[phi-var target] (find-candidate cfg adapter)]
      (recur (-> cfg
                 (raise-defs phi-var target adapter)
                 (replace-uses phi-var target adapter))
             (inc n))
      cfg)))
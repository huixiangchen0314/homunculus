(ns top.kzre.homunculus.core.cfg.pass.ssa.construct
  "SSA 构造：pruned phi 插入 + 变量重命名。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.analysis.dominator :as dom]
    [top.kzre.homunculus.core.cfg.analysis.liveness :as live]
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.phi :as phi])
  (:import (top.kzre.homunculus.core.cfg.model CFG)))

(defonce ^:private ^:const version-separator* "$$")
(defn version-separator [] version-separator*)

;; ═══════════════════════════════════════════
;; 版本命名
;; ═══════════════════════════════════════════

(defn- new-version
  "分配新版本。返回 {:version sym :counters map}。"
  [counters var-name]
  (let [n (inc (get counters var-name 0))]
    {:version  (symbol (namespace var-name)
                       (str (name var-name) (version-separator) n))
     :counters (assoc counters var-name n)}))

(defn strip-versions
  [sym]
  (let [s (name sym)
        i (str/last-index-of s (version-separator))]
    (if i
      (symbol (namespace sym) (subs s 0 i))
      sym)))

;; ═══════════════════════════════════════════
;; 版本栈
;; ═══════════════════════════════════════════

(defn- push-version
  [stacks var-name version]
  (update stacks var-name (fnil conj []) version))

(defn- pop-version
  [stacks var-name]
  (update stacks var-name
          (fnil #(if (seq %) (pop %) []) [])))

(defn- top-version
  [stacks var-name]
  (or (peek (get stacks var-name [])) var-name))

(defn- uses-substitution
  [stacks adapter stmt]
  (into {}
        (map (fn [var-name]
               [var-name (top-version stacks var-name)]))
        (adapt/stmt-uses adapter stmt)))

;; ═══════════════════════════════════════════
;; collect-defs
;; ═══════════════════════════════════════════

(defn- collect-defs
  [cfg adapter]
  (reduce
    (fn [acc [block-id block]]
      (reduce
        (fn [acc stmt]
          (reduce
            (fn [acc var-name]
              (update acc var-name
                      (fn [entry]
                        (let [existing (or entry {:blocks #{} :origin nil})]
                          {:blocks (conj (:blocks existing) block-id)
                           :origin (or (:origin existing) stmt)}))))
            acc
            (adapt/stmt-defs adapter stmt)))
        acc
        (:stmts block)))
    {}
    (:blocks cfg)))

;; ═══════════════════════════════════════════
;; Phi 插入（pruned SSA）
;; ═══════════════════════════════════════════

(defn- make-phis
  [^CFG cfg adapter dominance-frontier liveness]
  (let [definitions (collect-defs cfg adapter)]
    (reduce
      (fn [cfg [var-name {:keys [blocks origin]}]]
        (let [definition-blocks blocks]
          (loop [pending-blocks  (vec definition-blocks)
                 cfg             cfg
                 blocks-with-phi #{}]
            (if (empty? pending-blocks)
              cfg
              (let [current-block    (peek pending-blocks)
                    remaining-blocks (pop pending-blocks)
                    frontier-blocks  (get dominance-frontier current-block #{})
                    result
                    (reduce
                      (fn [{:keys [cfg pending with-phi]} frontier-block]
                        (cond
                          (contains? with-phi frontier-block)
                          {:cfg      cfg
                           :pending  pending
                           :with-phi with-phi}

                          (not (live/live-in? liveness frontier-block var-name))
                          {:cfg      cfg
                           :pending  pending
                           :with-phi with-phi}

                          :else
                          (let [phi-node     (phi/make-phi var-name origin)
                                cfg-next     (update-in cfg
                                                        [:blocks frontier-block :phis]
                                                        (fnil conj []) phi-node)
                                pending-next (if (contains? definition-blocks
                                                            frontier-block)
                                               pending
                                               (conj pending frontier-block))]
                            {:cfg      cfg-next
                             :pending  pending-next
                             :with-phi (conj with-phi frontier-block)})))
                      {:cfg      cfg
                       :pending  remaining-blocks
                       :with-phi blocks-with-phi}
                      frontier-blocks)]
                (recur (:pending result)
                       (:cfg result)
                       (:with-phi result)))))))
      cfg
      definitions)))

;; ═══════════════════════════════════════════
;; 重命名——每块的处理
;; ═══════════════════════════════════════════

(defn- push-phi-vars
  "phi-var 用 base name——push 到栈。
   返回 {:versioned [...] :stacks {...}}。"
  [cfg stacks block-id]
  (reduce (fn [{:keys [versioned stacks]} phi]
            (let [var-name (phi/phi-var-name phi)]
              {:versioned (conj versioned var-name)
               :stacks    (push-version stacks var-name var-name)}))
          {:versioned [] :stacks stacks}
          (:phis (cfg/cfg-block cfg block-id))))

(defn- rename-stmt
  "对单条 stmt 做 uses-subst 和 def 版本化。

   var-decl 不版本化——声明不是 def。只有 assign 的 lhs 版本化。

   返回 {:stmt ... :versioned [...] :stacks {...} :counters {...}}。"
  [stmt stacks counters adapter phi-var-names]
  (let [renamed-stmt (adapt/stmt-replace-uses
                       adapter stmt
                       (uses-substitution stacks adapter stmt))]
    (if (adapt/assign-stmt? adapter renamed-stmt)
      (let [definitions (adapt/stmt-defs adapter renamed-stmt)]
        (reduce
          (fn [{:keys [stmt versioned stacks counters]} definition]
            (if (contains? phi-var-names definition)
              (let [version-result (new-version counters definition)
                    version        (:version version-result)]
                {:stmt      (adapt/stmt-replace-defs
                              adapter stmt {definition version})
                 :versioned (conj versioned definition)
                 :stacks    (push-version stacks definition version)
                 :counters  (:counters version-result)})
              {:stmt      stmt
               :versioned versioned
               :stacks    stacks
               :counters  counters}))
          {:stmt      renamed-stmt
           :versioned []
           :stacks    stacks
           :counters  counters}
          definitions))
      {:stmt      renamed-stmt
       :versioned []
       :stacks    stacks
       :counters  counters})))

(defn- rename-stmts
  "返回 {:stmts [...] :versioned [...] :stacks {...} :counters {...}}。"
  [stmts stacks counters adapter phi-var-names]
  (reduce
    (fn [{:keys [stmts versioned stacks counters]} stmt]
      (let [result (rename-stmt stmt stacks counters adapter phi-var-names)]
        {:stmts     (conj stmts (:stmt result))
         :versioned (into versioned (:versioned result))
         :stacks    (:stacks result)
         :counters  (:counters result)}))
    {:stmts     []
     :versioned []
     :stacks    stacks
     :counters  counters}
    stmts))

(defn- rename-term
  [term stacks adapter]
  (adapt/term-replace-uses
    adapter term
    (into {} (map (fn [var-name]
                    [var-name (top-version stacks var-name)]))
          (adapt/term-uses adapter term))))

(defn- fill-successor-phi-inputs
  "对 block 的每个后继 phi，填充当前前驱的 input。"
  [^CFG cfg stacks block-id adapter]
  (reduce
    (fn [cfg successor-id]
      (update-in cfg [:blocks successor-id :phis]
                 (fn [phis]
                   (mapv (fn [p]
                           (let [var-name (top-version stacks (phi/phi-var-name p))
                                 ref      (adapt/make-var-ref
                                            adapter (phi/phi-origin p) var-name)]
                             (phi/update-phi-input p block-id ref var-name)))
                         phis))))
    cfg
    (cfg/cfg-successors-of cfg block-id)))

;; ═══════════════════════════════════════════
;; 重命名——DFS 支配树
;; ═══════════════════════════════════════════

(defn- version-rename
  "DFS 重命名 block 及其支配树。
   返回 {:cfg ... :stacks {...} :counters {...}}。"
  [cfg stacks counters dom-tree adapter phi-var-names block-id]
  (let [block               (cfg/cfg-block cfg block-id)
        pushed-phi-vars     (push-phi-vars cfg stacks block-id)
        renamed-stmts       (rename-stmts (:stmts block)
                                          (:stacks pushed-phi-vars)
                                          counters adapter phi-var-names)
        renamed-term        (rename-term (:term block)
                                         (:stacks renamed-stmts) adapter)
        cfg-with-block      (-> cfg
                                (assoc-in [:blocks block-id :stmts]
                                          (:stmts renamed-stmts))
                                (assoc-in [:blocks block-id :term]
                                          renamed-term))
        cfg-with-phi-inputs (fill-successor-phi-inputs
                              cfg-with-block
                              (:stacks renamed-stmts)
                              block-id adapter)
        child-result        (reduce
                              (fn [{:keys [cfg stacks counters]} child]
                                (version-rename cfg stacks counters dom-tree
                                                adapter phi-var-names child))
                              {:cfg      cfg-with-phi-inputs
                               :stacks   (:stacks renamed-stmts)
                               :counters (:counters renamed-stmts)}
                              (get dom-tree block-id))
        versioned-all       (into (:versioned pushed-phi-vars)
                                  (:versioned renamed-stmts))
        final-stacks        (reduce pop-version
                                    (:stacks child-result)
                                    (reverse versioned-all))]
    {:cfg      (:cfg child-result)
     :stacks   final-stacks
     :counters (:counters child-result)}))

(defn- make-versioned
  [^CFG cfg dom-tree adapter]
  (let [phi-var-names (set (map phi/phi-var-name
                                (mapcat :phis (vals (:blocks cfg)))))
        result        (version-rename cfg {} {} dom-tree adapter
                                      phi-var-names (:entry cfg))]
    (:cfg result)))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn construct
  [^CFG cfg adapter]
  (let [liveness              (live/cfg-liveness cfg adapter)
        {:keys [dominance-frontier dom-tree]} (dom/analyze cfg)
        cfg-phis              (make-phis cfg adapter dominance-frontier liveness)]
    (make-versioned cfg-phis dom-tree adapter)))
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
  "返回 [version-symbol counters']。

   保留原符号的命名空间——foo/bar → foo/bar$$1。"
  [counters var-name]
  (let [n (inc (get counters var-name 0))]
    [(symbol (namespace var-name)
             (str (name var-name) (version-separator) n))
     (assoc counters var-name n)]))

(defn strip-versions
  "去掉版本后缀，保留原符号的命名空间——foo/bar$$1 → foo/bar。"
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
  [stacks var-name v]
  (update stacks var-name (fnil conj []) v))

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
        (map (fn [v] [v (top-version stacks v)]))
        (adapt/stmt-uses adapter stmt)))

;; ═══════════════════════════════════════════
;; collect-defs
;; ═══════════════════════════════════════════

(defn- collect-defs
  [cfg adapter]
  (reduce
    (fn [acc [id block]]
      (reduce
        (fn [acc stmt]
          (reduce
            (fn [acc var-name]
              (update acc var-name
                      (fn [entry]
                        (let [e (or entry {:blocks #{} :origin nil})]
                          {:blocks (conj (:blocks e) id)
                           :origin (or (:origin e) stmt)}))))
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
  (let [definitions   (collect-defs cfg adapter)]
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
                    [cfg' pending-blocks' blocks-with-phi']
                    (reduce
                      (fn [[c pending with-phi] frontier-block]
                        (cond
                          (contains? with-phi frontier-block)
                          [c pending with-phi]

                          (not (live/live-in? liveness frontier-block var-name))
                          [c pending with-phi]

                          :else
                          (let [phi-node (phi/make-phi var-name origin)   ; ← 不传 predecessors
                                c'       (update-in c [:blocks frontier-block :phis]
                                                    (fnil conj []) phi-node)
                                pending' (if (contains? definition-blocks frontier-block)
                                           pending
                                           (conj pending frontier-block))]
                            [c' pending' (conj with-phi frontier-block)])))
                      [cfg remaining-blocks blocks-with-phi]
                      frontier-blocks)]
                (recur pending-blocks' cfg' blocks-with-phi'))))))
      cfg
      definitions)))

;; ═══════════════════════════════════════════
;; 重命名——每块的处理
;; ═══════════════════════════════════════════

(defn- push-phi-vars
  "phi-var 用 base name——push 到栈。
   返回 [versioned-var-names stacks']。"
  [cfg stacks block-id]
  (reduce (fn [[versioned stack] phi]
            (let [vn (phi/phi-var-name phi)]
              [(conj versioned vn) (push-version stack vn vn)]))
          [[] stacks]
          (:phis (cfg/cfg-block cfg block-id))))

(defn- rename-stmt
  "对单条 stmt 做 uses-subst 和 def 版本化。

   var-decl 不版本化——声明不是 def。只有 assign 的 lhs 版本化。"
  [stmt stacks counters adapter phi-var-names]
  (let [stmt' (adapt/stmt-replace-uses
                adapter stmt
                (uses-substitution stacks adapter stmt))]
    (if (adapt/assign-stmt? adapter stmt')
      (let [defs (adapt/stmt-defs adapter stmt')]
        (reduce
          (fn [[s versioned st ct] d]
            (if (contains? phi-var-names d)
              (let [[v ct'] (new-version ct d)]
                [(adapt/stmt-replace-defs adapter s {d v})
                 (conj versioned d)
                 (push-version st d v)
                 ct'])
              [s versioned st ct]))
          [stmt' [] stacks counters]
          defs))
      [stmt' [] stacks counters])))

(defn- rename-stmts
  "返回 [stmts' versioned-var-names stacks' counters']。"
  [stmts stacks counters adapter phi-var-names]
  (reduce
    (fn [[acc versioned st ct] stmt]
      (let [[stmt' versioned' st' ct']
            (rename-stmt stmt st ct adapter phi-var-names)]
        [(conj acc stmt') (into versioned versioned') st' ct']))
    [[] [] stacks counters]
    stmts))

(defn- rename-term
  [term stacks adapter]
  (adapt/term-replace-uses
    adapter term
    (into {} (map (fn [v] [v (top-version stacks v)]))
          (adapt/term-uses adapter term))))

(defn- fill-successor-phi-inputs
  "对 block 的每个后继 phi，填充当前前驱的 input。"
  [^CFG cfg stacks block-id adapter]
  (reduce
    (fn [c successor-id]
      (update-in c [:blocks successor-id :phis]
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
   返回 [cfg' stacks' counters']。"
  [^CFG cfg stacks counters dom-tree adapter phi-var-names id]
  (let [block                          (cfg/cfg-block cfg id)
        [versioned-phi stacks1]        (push-phi-vars cfg stacks id)
        [new-stmts versioned-stmts stacks2 counters1]
        (rename-stmts (:stmts block) stacks1 counters adapter phi-var-names)
        term'                          (rename-term (:term block) stacks2 adapter)
        cfg1                           (-> cfg
                                           (assoc-in [:blocks id :stmts] new-stmts)
                                           (assoc-in [:blocks id :term] term'))
        cfg2                           (fill-successor-phi-inputs cfg1 stacks2 id adapter)
        [cfg3 stacks3 counters2]
        (reduce (fn [[c st ct] child]
                  (version-rename c st ct dom-tree adapter phi-var-names child))
                [cfg2 stacks2 counters1]
                (get dom-tree id))
        versioned-all                  (into versioned-phi versioned-stmts)
        stacks4                        (reduce pop-version stacks3 (reverse versioned-all))]
    [cfg3 stacks4 counters2]))

(defn- make-versioned
  [^CFG cfg dom-tree adapter]
  (let [phi-var-names (set (map phi/phi-var-name
                                (mapcat :phis (vals (:blocks cfg)))))
        [cfg' _ _]    (version-rename cfg {} {} dom-tree adapter
                                    phi-var-names (:entry cfg))]
    cfg'))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn construct
  [^CFG cfg adapter]
  (let [liveness (live/cfg-liveness cfg adapter)
        {:keys [dominance-frontier
                dom-tree]} (dom/analyze cfg)
        cfg-phis (make-phis cfg adapter dominance-frontier liveness)]
    (make-versioned cfg-phis dom-tree adapter)))
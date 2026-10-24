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

(defn- next-version
  [counters var]
  (let [n (get @counters var 0)]
    (swap! counters assoc var (inc n))
    (symbol (str (name var) (version-separator) (inc n)))))

(defn strip-versions
  [sym]
  (let [s (name sym)
        i (str/last-index-of s (version-separator))]
    (if i (symbol (subs s 0 i)) sym)))

;; ═══════════════════════════════════════════
;; collect-defs——收集定义块 + template
;; ═══════════════════════════════════════════

(defn- collect-defs
  "返回 {var → {:blocks #{block-id ...} :template def-stmt}}。

   template 是**第一次定义该变量的 stmt**——保证非 nil
   （因为 stmt 来自 :stmts block）。"
  [cfg adapter]
  (reduce
    (fn [acc [id block]]
      (reduce
        (fn [acc stmt]
          (reduce
            (fn [acc v]
              (update acc v
                      (fn [entry]
                        (let [e (or entry {:blocks #{} :template nil})]
                          {:blocks   (conj (:blocks e) id)
                           :template (or (:template e) stmt)}))))
            acc
            (adapt/stmt-defs adapter stmt)))
        acc
        (:stmts block)))
    {}
    (:blocks cfg)))

;; ═══════════════════════════════════════════
;; Phi 插入（pruned SSA）
;; ═══════════════════════════════════════════

(defn- insert-phi-nodes
  [cfg adapter df liveness]
  (let [preds (cfg/cfg-build-preds cfg)
        defs  (collect-defs cfg adapter)]
    (reduce
      (fn [cfg [var {:keys [blocks template]}]]
        (let [def-blocks blocks]
          (loop [worklist (vec def-blocks)
                 cfg      cfg
                 has-phi  #{}]
            (if (empty? worklist)
              cfg
              (let [b         (peek worklist)
                    worklist' (pop worklist)
                    df-blocks (get df b #{})
                    [cfg' worklist'' has-phi']
                    (reduce
                      (fn [[c w h] d]
                        (cond
                          (contains? h d)
                          [c w h]

                          (not (live/live-in? liveness d var))
                          [c w h]

                          :else
                          (let [p  (phi/make-phi var template (get preds d []))
                                c' (update-in c [:blocks d :phis]
                                              (fnil conj []) p)
                                w' (if (contains? def-blocks d)
                                     w
                                     (conj w d))]
                            [c' w' (conj h d)])))
                      [cfg worklist' has-phi]
                      df-blocks)]
                (recur worklist'' cfg' has-phi'))))))
      cfg
      defs)))

;; ═══════════════════════════════════════════
;; 变量重命名（DFS 支配树）
;; ═══════════════════════════════════════════

(defn- rename-variables
  [cfg dom-tree adapter]
  (let [stacks   (atom {})
        counters (atom {})
        cfg-atom (atom cfg)]
    (letfn [(push! [var v] (swap! stacks update var (fnil conj []) v))
            (pop!  [var]   (swap! stacks update var
                                  (fnil #(if (seq %) (pop %) []) [])))
            (top   [var]   (or (peek (get @stacks var [])) var))

            (uses-subst [stmt]
              (into {}
                    (map (fn [v] [v (top v)]))
                    (adapt/stmt-uses adapter stmt)))

            (rename-block [id]
              (let [pushed (atom [])
                    block  (cfg/cfg-block @cfg-atom id)
                    phis   (:phis block)]

                ;; ── 1. phis：压新版本，重命名 phi var ──
                (when (seq phis)
                  (let [new-vars (mapv (fn [p]
                                         (let [var (phi/phi-var p)
                                               v   (next-version counters var)]
                                           (push! var v)
                                           (swap! pushed conj var)
                                           v))
                                       phis)]
                    (swap! cfg-atom update-in [:blocks id :phis]
                           (fn [old-phis]
                             (mapv (fn [p v] (phi/rename-phi-var p v))
                                   old-phis new-vars)))))

                ;; ── 2. stmts ──
                (let [block (cfg/cfg-block @cfg-atom id)
                      new-stmts
                      (mapv (fn [stmt]
                              (let [stmt' (adapt/stmt-replace-uses
                                            adapter stmt (uses-subst stmt))
                                    defs  (adapt/stmt-defs adapter stmt')]
                                (reduce
                                  (fn [s d]
                                    (let [v (next-version counters d)]
                                      (push! d v)
                                      (swap! pushed conj d)
                                      (adapt/stmt-replace-defs
                                        adapter s {d v})))
                                  stmt' defs)))
                            (:stmts block))]
                  (swap! cfg-atom assoc-in [:blocks id :stmts] new-stmts))

                ;; ── 3. term ──
                (let [term  (:term (cfg/cfg-block @cfg-atom id))
                      subst (into {} (map (fn [v] [v (top v)]))
                                  (adapt/term-uses adapter term))
                      term' (adapt/term-replace-uses adapter term subst)]
                  (swap! cfg-atom assoc-in [:blocks id :term] term'))

                ;; ── 4. 后继 phi 输入——从 template 继承 attrs ──
                (doseq [s (cfg/cfg-successors-of @cfg-atom id)]
                  (swap! cfg-atom update-in [:blocks s :phis]
                         (fn [phis]
                           (mapv (fn [p]
                                   (let [tmpl (phi/phi-template p)
                                         base (strip-versions (phi/phi-var p))
                                         v    (top base)
                                         ref  (adapt/make-var-ref adapter tmpl v)]
                                     (phi/update-phi-input p id ref)))
                                 phis))))

                ;; ── 5. 递归支配树 ──
                (doseq [child (get dom-tree id)]
                  (rename-block child))

                ;; ── 6. 弹出本块版本 ──
                (doseq [var (reverse @pushed)]
                  (pop! var))))]
      (rename-block (:entry cfg))
      @cfg-atom)))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn construct
  [^CFG cfg adapter]
  (let [liveness              (live/cfg-liveness cfg adapter)
        {:keys [df dom-tree]} (dom/analyze cfg)
        cfg-phis              (insert-phi-nodes cfg adapter df liveness)]
    (rename-variables cfg-phis dom-tree adapter)))
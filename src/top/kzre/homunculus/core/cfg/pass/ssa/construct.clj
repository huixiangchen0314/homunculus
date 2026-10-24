(ns top.kzre.homunculus.core.cfg.pass.ssa.construct
  "SSA 构造：pruned phi 插入 + 变量重命名。

   ── 版本命名 ────────────────────────────────────────

   版本用 `$$N` 后缀：x → x$$1 / x$$2 / ...。`$$` 不与原变量名的
   `_N` 混淆——strip-versions 时按 `$$` 分割，原名字的 `_N` 不受
   影响。

   ── 未定义变量的处理 ────────────────────────────────

   变量在版本栈为空时（未定义），`top` 返回原名——函数参数、全局
   uniform 保持原名，不参与重命名。这与 SSA 的\"外部变量\"语义一致
   ——它们在函数入口处已有值，不需要版本化。

   ── Phi 输入的填充 ──────────────────────────────────

   处理块 B 的 stmts 和 term 后，B 的版本栈顶就是\"B 末尾的版本\"。
   对 B 在 CFG 上的所有后继 S，S 的 phi 中 pred=B 的输入填这个
   版本。这保证了 back-edge（循环）的 phi 输入是循环体末尾的版本。

   ── 不可达块 ────────────────────────────────────────

   CFG 构造时已丢弃不可达代码——所有块可达，所有前驱都会在 DFS
   中被访问到。phi 输入的 value 都会被真实填上。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.core.cfg.analysis.dominator :as dom]
    [top.kzre.homunculus.core.cfg.analysis.liveness :as live]
    [top.kzre.homunculus.core.cfg.analysis.use-def :as use-def]
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.phi :as phi]))

;; ═══════════════════════════════════════════
;; 版本命名
;; ═══════════════════════════════════════════

(defn- next-version
  [counters var]
  (let [n (get @counters var 0)]
    (swap! counters assoc var (inc n))
    (symbol (str (name var) "$$" (inc n)))))

(defn strip-versions
  "去版本化——x$$N → x。$$ 之前的部分。无 $$ 时原样返回。"
  [sym]
  (let [s (name sym)
        i (str/index-of s "$$")]
    (if i
      (symbol (subs s 0 i))
      sym)))

;; ═══════════════════════════════════════════
;; Phi 插入（pruned SSA）
;; ═══════════════════════════════════════════

(defn- collect-defs
  "每个变量的定义块集合。{var → #{block-id ...}}"
  [cfg adapter]
  (reduce
    (fn [acc [id block]]
      (reduce
        (fn [acc stmt]
          (reduce (fn [acc v] (update acc v (fnil conj #{}) id))
                  acc
                  (use-def/stmt-defs adapter stmt)))
        acc
        (:stmts block)))
    {}
    (:blocks cfg)))

(defn- insert-phi-nodes
  [cfg adapter df liveness]
  (let [preds (cfg/cfg-build-preds cfg)
        defs  (collect-defs cfg adapter)]
    (reduce
      (fn [cfg [var def-blocks]]
        (loop [worklist (vec def-blocks)
               cfg      cfg
               has-phi  #{}]
          (if (empty? worklist)
            cfg
            (let [b         (peek worklist)
                  worklist' (pop worklist)
                  df-blocks (get df b #{})
                  ;; 内层 reduce 返回 [cfg' worklist'' has-phi']
                  [cfg' worklist'' has-phi']
                  (reduce
                    (fn [[c w h] d]
                      (cond
                        (contains? h d)
                        [c w h]

                        (not (live/live-in? liveness d var))
                        [c w h]

                        :else
                        (let [p  (phi/make-phi var (get preds d []))
                              c' (update-in c [:blocks d :phis]
                                            (fnil conj []) p)
                              w' (if (contains? def-blocks d)
                                   w
                                   (conj w d))]
                          [c' w' (conj h d)])))
                    [cfg worklist' has-phi]
                    df-blocks)]
              ;; ←：recur 用 cfg'，不是 [cfg' worklist'' has-phi']
              (recur worklist'' cfg' has-phi')))))
      cfg
      defs)))

;; ═══════════════════════════════════════════
;; 变量重命名（DFS 支配树）
;; ═══════════════════════════════════════════

(defn- rename-variables
  [cfg dom-tree adapter]
  (let [stacks   (atom {})   ; {var → [version ...]}
        counters (atom {})   ; {var → n}
        cfg-atom (atom cfg)]
    (letfn [(push! [var v] (swap! stacks update var (fnil conj []) v))
            (pop!  [var]
              (swap! stacks update var
                     (fnil #(if (seq %) (pop %) []) [])))
            (top   [var]   (or (peek (get @stacks var [])) var))

            (uses-subst [stmt]
              (into {}
                    (map (fn [v] [v (top v)]))
                    (use-def/stmt-uses adapter stmt)))

            (rename-block [id]
              (let [pushed (atom [])
                    block  (cfg/cfg-block @cfg-atom id)
                    phis   (:phis block)]

                ;; ── 1. phis：压新版本、重命名 phi 的 var ──
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
                             (mapv (fn [p v]
                                     (phi/rename-phi-var p v))
                                   old-phis new-vars)))))

                ;; ── 2. stmts：先用旧版本替换 uses，再压新版本替换 defs ──
                (let [block (cfg/cfg-block @cfg-atom id)
                      new-stmts
                      (mapv (fn [stmt]
                              (let [stmt' (use-def/stmt-replace-uses
                                            adapter stmt (uses-subst stmt))
                                    defs  (use-def/stmt-defs adapter stmt')]
                                (reduce
                                  (fn [s d]
                                    (let [v (next-version counters d)]
                                      (push! d v)
                                      (swap! pushed conj d)
                                      (use-def/stmt-replace-defs
                                        adapter s {d v})))
                                  stmt' defs)))
                            (:stmts block))]
                  (swap! cfg-atom assoc-in [:blocks id :stmts] new-stmts))

                ;; ── 3. term：替换 uses ──
                (let [term  (:term (cfg/cfg-block @cfg-atom id))
                      subst (into {} (map (fn [v] [v (top v)]))
                                  (use-def/term-uses adapter term))
                      term' (use-def/term-replace-uses adapter term subst)]
                  (swap! cfg-atom assoc-in [:blocks id :term] term'))

                ;; ── 4. 填后继 phi 的输入：当前块末尾版本 ──
                (doseq [s (cfg/cfg-successors-of @cfg-atom id)]
                  (swap! cfg-atom update-in [:blocks s :phis]
                         (fn [phis]
                           (mapv (fn [p]
                                   (let [var (phi/phi-var p)
                                         base (strip-versions var)  ; ← 原变量名
                                         v    (top base)
                                         ref  (use-def/make-variable-ref adapter v)]
                                     (phi/update-phi-input p id ref)))
                                 phis))))

                ;; ── 5. 递归支配树子节点 ──
                (doseq [child (get dom-tree id)]
                  (rename-block child))

                ;; ── 6. 弹出本块压的版本 ──
                (doseq [var (reverse @pushed)]
                  (pop! var))))]
      (rename-block (:entry cfg))
      @cfg-atom)))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn construct
  "CFG → SSA-CFG。

   1. 计算 liveness（pruned SSA 的判据）
   2. 计算支配树和支配边界
   3. 插入 phi 节点
   4. DFS 支配树重命名变量"
  [cfg adapter]
  (let [liveness              (live/compute-liveness cfg adapter)
        {:keys [df dom-tree]} (dom/analyze cfg)
        cfg-phis              (insert-phi-nodes cfg adapter df liveness)]
    (rename-variables cfg-phis dom-tree adapter)))
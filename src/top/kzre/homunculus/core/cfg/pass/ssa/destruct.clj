(ns top.kzre.homunculus.core.cfg.pass.ssa.destruct
  "Phi 消除——SSA-CFG → 普通 CFG。

   ▸ 方法 1（return 中转消除）：块 B 的 term 是 :return，值追溯到
     某个 phi（直接或通过别名 assign），B 的所有前驱是 :jump B，
     B 的 stmts 只含别名 assign（最多 1 条）——各前驱 term 改成
     :return phi-input，删除 B。迭代到不动点。

   ▸ 方法 2（并行复制 fallback）：剩余 phi 各前驱末尾插入 move。"
  (:require
    [top.kzre.homunculus.core.cfg.adapt :as adapt]
    [top.kzre.homunculus.core.cfg.model :as cfg]
    [top.kzre.homunculus.core.cfg.phi :as phi]
    [top.kzre.homunculus.core.cfg.protocol :as proto]
    [top.kzre.homunculus.core.irstmt.ast :as ast]))

;; ═══════════════════════════════════════════
;; 方法 1：return 中转消除
;; ═══════════════════════════════════════════

(defn- trace-return-to-phi
  "从 B 的 return 值追溯到 phi。

   返回 {:phi p :alias-assign stmt|nil} 或 nil：
   ▸ return 直接是 phi-var                   → {:phi p :alias-assign nil}
   ▸ return 是 v，且 stmts 有 v = phi-var    → {:phi p :alias-assign 那条}
   ▸ 其他                                      → nil"
  [block]
  (let [term     (:term block)
        stmts    (:stmts block)
        phis     (:phis block)
        phi-vars (set (map phi/phi-var phis))
        v-name   (when (= :return (proto/term-kind term))
                   (let [v (:value term)]
                     (when (and v (= :variable (ast/kind v)))
                       (:name v))))]
    (when v-name
      (cond
        (contains? phi-vars v-name)
        {:phi (some #(when (= v-name (phi/phi-var %)) %) phis)
         :alias-assign nil}

        :else
        (when-let [alias-assign
                   (some (fn [stmt]
                           (when (and (= :assign (ast/kind stmt))
                                      (= v-name (:name (:var stmt)))
                                      (= :variable (ast/kind (:val stmt)))
                                      (contains? phi-vars (:name (:val stmt))))
                             stmt))
                         stmts)]
          (let [alias-var (:name (:val alias-assign))]
            {:phi (some #(when (= alias-var (phi/phi-var %)) %) phis)
             :alias-assign alias-assign}))))))

(defn- eligible-for-return-elimination?
  [ssa-cfg id]
  (let [block  (cfg/cfg-block ssa-cfg id)
        stmts  (:stmts block)
        preds  (get (cfg/cfg-build-preds ssa-cfg) id [])
        traced (trace-return-to-phi block)]
    (and traced
         (let [cnt (count stmts)]
           (or (zero? cnt)
               (and (= 1 cnt) (some? (:alias-assign traced)))))
         (every? #(= :jump (proto/term-kind (:term (cfg/cfg-block ssa-cfg %))))
                 preds))))

(defn- eliminate-via-return
  "方法 1：各前驱的 term 改成 :return phi-input，删除 B。

   ⚠️ lambda 里 :pred 解构必须改名 input-pred——外层 pred 是参数，
   被 :keys 解构遮蔽会导致 (= pred pred) 恒 true。"
  [ssa-cfg id]
  (let [block      (cfg/cfg-block ssa-cfg id)
        traced     (trace-return-to-phi block)
        target-phi (:phi traced)
        preds      (get (cfg/cfg-build-preds ssa-cfg) id [])]
    (reduce
      (fn [cfg' pred]
        (let [input    (some (fn [{input-pred :pred :keys [value]}]
                               (when (= pred input-pred) value))
                             (phi/phi-inputs target-phi))
              new-term (cfg/->Return input)]
          (cfg/cfg-replace-term cfg' pred new-term)))
      (cfg/cfg-remove-block ssa-cfg id)
      preds)))

(defn- eliminate-method-1-fixpoint
  [ssa-cfg]
  (loop [c ssa-cfg]
    (let [candidates (filterv #(eligible-for-return-elimination? c %)
                              (keys (:blocks c)))]
      (if (empty? candidates)
        c
        (recur (reduce eliminate-via-return c candidates))))))

;; ═══════════════════════════════════════════
;; 方法 2：并行复制
;; ═══════════════════════════════════════════

(defn- eliminate-phi-block
  "各前驱末尾插入 move。move 构造走 adapt/make-move——
   template 从 phi 继承，保证新 assign 的 var 带 attrs。"
  [cfg id adapter]
  (let [block (cfg/cfg-block cfg id)
        phis  (:phis block)]
    (if (empty? phis)
      cfg
      (reduce
        (fn [c p]
          (let [var  (phi/phi-var p)
                tmpl (phi/phi-template p)]
            (reduce
              (fn [c {:keys [value pred]}]
                (let [move (adapt/make-move adapter tmpl var value)]
                  (update-in c [:blocks pred :stmts] conj move)))
              c
              (phi/phi-inputs p))))
        cfg
        phis))))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn elim-phi
  "消除所有 phi 节点。

   只用方法 2——保留 merge 块，phi 消除转 move。

   方法 1（return 中转消除）暂不启用——它产生\"if 分支各自 return\"
   的 CFG 形态，emit 层不支持（if 的 then/else Block 的 ret 被
   emit-block-body 在非 top-level 时忽略）。方法 2 保留 merge 块，
   phi 消除转 move——bake 出汇合 if，emit 正确。

   方法 1 相关函数（trace-return-to-phi / eligible-for-return-
   elimination? / eliminate-via-return / eliminate-method-1-fixpoint）
   保留，将来 emit 层支持 if 分支 return 后可以重新启用。"
  [ssa-cfg adapter]
  (let [after-method-2 (reduce
                         (fn [c id]
                           (if (:phis (cfg/cfg-block c id))
                             (eliminate-phi-block c id adapter)
                             c))
                         ssa-cfg
                         (keys (:blocks ssa-cfg)))]
    (update after-method-2 :blocks
            (fn [blocks]
              (into {}
                    (map (fn [[id b]] [id (assoc b :phis nil)]))
                    blocks)))))
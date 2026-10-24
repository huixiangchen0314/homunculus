(ns top.kzre.homunculus.core.cfg.model
  "CFG 的 record 定义与基础操作。

   ── 边的建模 ──

   边不是独立对象——由 terminator 的 target / then / else 隐含表达。
   `:jump` 有一条出边，`:branch` 有两条，`:return` / `:throw` 无出边。
   前驱关系通过遍历所有块构建（cfg-build-preds）。

   这个设计保证\"边索引\"和\"终结指令\"永远一致——
   不可能出现\"边存在但 term 不指向\"或\"term 指向但边不存在\"。"
  (:require
    [top.kzre.homunculus.core.cfg.protocol :as proto]))

;; ═══════════════════════════════════════════
;; 终结指令
;; ═══════════════════════════════════════════

(defrecord Jump [target]
  proto/ITerminator
  (term-kind [_] :jump)
  (term-successors [_] [target]))

(defrecord Branch [test then else]
  proto/ITerminator
  (term-kind [_] :branch)
  (term-successors [_] [then else]))

(defrecord Return [value]
  proto/ITerminator
  (term-kind [_] :return)
  (term-successors [_] []))

(defrecord Throw [value]
  proto/ITerminator
  (term-kind [_] :throw)
  (term-successors [_] []))

;; ═══════════════════════════════════════════
;; 基本块
;; ═══════════════════════════════════════════

(defrecord BasicBlock
  [id stmts term origin
   phis]
  proto/IBasicBlock
  (bb-id [_] id)
  (bb-stmts [_] stmts)
  (bb-term [_] term)
  (bb-origin [_] origin)
  (bb-successors [_] (proto/term-successors term)))

;; ═══════════════════════════════════════════
;; CFG
;; ═══════════════════════════════════════════

(defrecord CFG [entry blocks]
  ;; entry  —— 入口块 id（必在 blocks 里）
  ;; blocks —— {id → BasicBlock}
  ;;
  ;; 没有 exit 字段——终止块由 cfg-terminators 派生。
  ;; 这样避免虚拟 exit 块的额外边，数据流分析直接以终止块为边界。
  )

;; ═══════════════════════════════════════════
;; 构造
;; ═══════════════════════════════════════════

(defn make-block
  "构造基本块。stmts 会被 vec 化。

   origin 可选——:if / :while / :block 节点时传入，其他情况下为 nil。"
  ([id stmts term] (->BasicBlock id (vec stmts) term nil nil))
  ([id stmts term origin] (->BasicBlock id (vec stmts) term origin nil)))

(defn make-cfg
  "从 entry 和块序列构造 CFG。块 id 必须唯一。"
  [entry blocks]
  (let [m (into {} (map (fn [b] [(proto/bb-id b) b]) blocks))]
    (when (not= (count m) (count blocks))
      (throw (ex-info "duplicate block id in CFG"
                      {:blocks (mapv proto/bb-id blocks)})))
    (when-not (contains? m entry)
      (throw (ex-info "entry block id not in blocks"
                      {:entry entry :ids (keys m)})))
    (->CFG entry m)))

;; ═══════════════════════════════════════════
;; 访问
;; ═══════════════════════════════════════════

(defn cfg-block
  [cfg id]
  (get (:blocks cfg) id))

(defn cfg-entry-block
  [cfg]
  (cfg-block cfg (:entry cfg)))

(defn cfg-block-ids
  [cfg]
  (keys (:blocks cfg)))

(defn cfg-successors-of
  [cfg id]
  (some-> (cfg-block cfg id) proto/bb-successors))

(defn cfg-predecessors-of
  "遍历所有块计算前驱。O(n) per call。
   频繁调用时用 cfg-build-preds 一次构建索引。"
  [cfg id]
  (->> (:blocks cfg)
       vals
       (filter (fn [b] (some #{id} (proto/bb-successors b))))
       (mapv proto/bb-id)))

(defn cfg-build-preds
  "构建 {id → [pred-id ...]} 索引。一次性 O(n+m)。"
  [cfg]
  (reduce (fn [acc b]
            (let [from (proto/bb-id b)]
              (reduce (fn [a to]
                        (update a to (fnil conj []) from))
                      acc
                      (proto/bb-successors b))))
          {}
          (vals (:blocks cfg))))

(defn cfg-build-succs
  "构建 {id → [succ-id ...]} 索引。"
  [cfg]
  (into {}
        (map (fn [[id b]]
               [id (vec (proto/bb-successors b))]))
        (:blocks cfg)))

(defn cfg-terminators
  "所有终止块（:return / :throw）的 id 集合。
   数据流分析的后向起点。"
  [cfg]
  (->> (:blocks cfg)
       vals
       (filter (fn [b] (contains? #{:return :throw}
                                  (proto/term-kind (proto/bb-term b)))))
       (map proto/bb-id)
       set))

(defn cfg-reachable?
  "id 是否从 entry 可达。BFS。"
  [cfg id]
  (loop [seen #{}
         work [( :entry cfg)]]
    (cond
      (empty? work)          false
      (contains? seen id)    true
      (contains? seen (first work)) (recur seen (rest work))
      :else
      (let [cur (first work)
            seen' (conj seen cur)
            succs (cfg-successors-of cfg cur)]
        (recur seen' (into (rest work) succs))))))

;; ═══════════════════════════════════════════
;; 修改
;; ═══════════════════════════════════════════

(defn cfg-add-block
  "添加块。同 id 已存在时抛异常。"
  [cfg block]
  (let [id (proto/bb-id block)]
    (when (contains? (:blocks cfg) id)
      (throw (ex-info "block id already exists"
                      {:id id})))
    (update cfg :blocks assoc id block)))

(defn cfg-remove-block
  "删除块。

   ⚠️ 不检查边的完整性——调用方负责处理指向此块的边
   （重定向到 other 或一并删除）。"
  [cfg id]
  (update cfg :blocks dissoc id))

(defn cfg-replace-block
  "替换块（同 id，内容不同）。"
  [cfg block]
  (let [id (proto/bb-id block)]
    (when-not (contains? (:blocks cfg) id)
      (throw (ex-info "block id not found" {:id id})))
    (update cfg :blocks assoc id block)))

(defn cfg-replace-term
  "替换块的终结指令。"
  [cfg id new-term]
  (update-in cfg [:blocks id]
             (fn [b] (assoc b :term new-term))))

(defn cfg-update-stmts
  "替换块的语句序列。stmts 会被 vec 化。"
  [cfg id new-stmts]
  (update-in cfg [:blocks id]
             (fn [b] (assoc b :stmts (vec new-stmts)))))

(defn cfg-replace-successor
  "在 id 块的 terminator 里，把 old-succ 替换成 new-succ。

   用于块删除 / 块合并时的边重定向。
   :jump 的 target 或 :branch 的 then/else 匹配 old-succ 时替换。"
  [cfg id old-succ new-succ]
  (update-in cfg [:blocks id :term]
             (fn [term]
               (case (proto/term-kind term)
                 :jump   (if (= (:target term) old-succ)
                           (assoc term :target new-succ)
                           term)
                 :branch (cond-> term
                                 (= (:then term) old-succ) (assoc :then new-succ)
                                 (= (:else term) old-succ) (assoc :else new-succ))
                 ;; :return / :throw 无后继
                 term))))
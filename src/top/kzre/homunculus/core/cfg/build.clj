(ns top.kzre.homunculus.core.cfg.build
  "IRStmt → CFG 构造。

   ── 规范形式 ──

   decon 返回：
     ▸ {:kind :if    :test expr :then block-form :else block-form|nil :origin node}
     ▸ {:kind :while :test expr :body block-form :origin node}
     ▸ {:kind :throw :expr expr :origin node}
     ▸ {:kind :block :stmts [...] :ret expr|nil :origin node}
     ▸ {:kind :stmt}

   :origin 是原 AST 节点——用于 CFG BasicBlock 的 origin，供 bake
   时构造带 attrs 的 IRStmt 节点。block-form 里的 :origin 是分支
   （then / else / body）的 :block 节点，decon 顶层的 :origin 是
   if / while / throw 节点本身。

   ── 调用方契约 ──

   build-cfg 接受 IRStmt 节点，内部通过 decon 协议拿规范形式。"
  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]))

;; ═══════════════════════════════════════════
;; 协议
;; ═══════════════════════════════════════════

(defprotocol ICFGBuilder
  (decon
    [_ node]
    "解构节点为规范形式之一（含 :origin 字段）：

     ▸ {:kind :if    :test expr :then block-form :else block-form|nil :origin node}
     ▸ {:kind :while :test expr :body block-form :origin node}
     ▸ {:kind :throw :expr expr :origin node}
     ▸ {:kind :block :stmts [stmt...] :ret expr|nil :origin node}
     ▸ {:kind :stmt}"))

;; ═══════════════════════════════════════════
;; 内部状态
;; ═══════════════════════════════════════════
(defrecord State [blocks
                  ^long next-id
                  current-id
                  current-stmts
                  current-origin
                  loop-heads])
(defn make-state
  ([]
   (map->State {:blocks {}                                  ;;id->block
                :next-id 0
                :loop-heads #{}})))

(defn- inc-next-id [^State state]
  (update state :next-id inc))

(defn- fresh-id [^State state]
  [(:next-id state) (inc-next-id state )])

(defn- with-current [^State state id origin]
  (assoc state :current-id id :current-stmts [] :current-origin origin))

(defn- set-origin
  "改当前块的 origin，不清空 stmts——用于 handle-if / handle-while
   在提交 branch 之前把 origin 设为 if / while 节点。"
  [^State state origin]
  (assoc state :current-origin origin))

(defn- append-stmt [^State state stmt]
  (update state :current-stmts conj stmt))

(defn- commit-with-term [^State state term]
  (if (nil? (:current-id state))
    state
    (let [id    (:current-id state)
          block (cfg/make-block id (:current-stmts state) term (:current-origin state))]
      (-> state
          (update :blocks assoc id block)
          (assoc :current-id nil :current-stmts [])))))

(defn- close-with-jump [^State state target]
  (if (nil? (:current-id state))
    state
    (commit-with-term state (cfg/->Jump target))))

(defn- close-with-return [state value]
  (if (nil? (:current-id state))
    state
    (commit-with-term state (cfg/->Return value))))

;; ═══════════════════════════════════════════
;; 构造核心
;; ═══════════════════════════════════════════

(declare build-stmts)

(defn- build-block
  "处理规范 block 形式。

   如果当前块 origin 是 nil 且 block-form 有 :origin，用它填充——
   处理函数 body 的 :block 节点展开到 entry 块的情况。"
  [state {:keys [stmts ret origin]} builder return?]
  (let [state0
        (if (and (nil? (:current-origin state)) origin)
          (assoc state :current-origin origin)
          state)
        state1 (build-stmts state0 stmts builder)]
    (cond
      (nil? (:current-id state1)) state1
      (nil? ret) state1
      return? (close-with-return state1 ret)
      :else (append-stmt state1 ret))))

(defn- handle-if
  [^State state
   {:keys [test then else origin]}
   builder]
  (let [parent-origin (:current-origin state)
        has-else?     (some? else)
        [then-id  state0] (fresh-id state)
        [else-id  state1] (if has-else? (fresh-id state0) [nil state0])
        [after-id state2] (fresh-id state1)

        ;; branch 块——包含 if 之前的 stmts + test
        ;; origin = if 节点本身——bake 时作为 If 节点 attrs 来源
        st-commited (commit-with-term
                (set-origin state2 origin)
                (cfg/->Branch test then-id (or else-id after-id)))

        ;; then 块——origin = then 分支的 :block 节点
        st-then0 (with-current st-commited then-id (:origin then))
        st-then1 (build-block st-then0 then builder false)
        st-then2 (close-with-jump st-then1 after-id)

        ;; else 块——origin = else 分支的 :block 节点
        st-else (if has-else?
                (let [st0 (with-current st-then2 else-id (:origin else))
                      st1 (build-block st0 else builder false)]
                  (close-with-jump st1 after-id))
                st-then2)

        ;; after 块——origin 是父 block
        st-after (with-current st-else after-id parent-origin)]
    st-after))

(defn- handle-while
  [^State state
   {:keys [test body origin]}
   builder]
  (let [parent-origin (:current-origin state)
        [head-id st-head] (fresh-id state)
        [body-id st-body] (fresh-id st-head)
        [exit-id st-exit] (fresh-id st-body)
        st-loop (close-with-jump st-exit head-id)

        ;; head 块——origin = while 节点
        st-head-b (with-current st-loop head-id origin)
        st-head-c (commit-with-term st-head-b (cfg/->Branch test body-id exit-id))

        ;; body 块——origin = body 分支的 :block 节点
        st-body-b (with-current st-head-c body-id (:origin body))
        st-body-c (build-block st-body-b body builder false)
        st-body-d (close-with-jump st-body-c head-id)

        ;; exit 块——origin 是父 block
        st-exit-b (with-current st-body-d exit-id parent-origin)]
    ;; 记录 head-id——bake 时直接查，不做回边检测
    (update st-exit-b :loop-heads (fnil conj #{}) head-id)))

(defn- handle-throw
  [^State state {:keys [expr origin]} _builder]
  (commit-with-term (set-origin state origin)
                    (cfg/->Throw expr)))

(defn- build-stmt
  [^State state stmt builder]
  (if (nil? (:current-id state))
    state
    (let [d (decon builder stmt)]
      (case (:kind d)
        :if    (handle-if    state d builder)
        :while (handle-while state d builder)
        :throw (handle-throw state d builder)
        :block (build-block  state d builder false)
        :stmt  (append-stmt  state stmt)
        (throw (ex-info "Unknown stmt kind in CFG build"
                        {:stmt stmt :kind (:kind d)}))))))

(defn- build-stmts
  [^State state stmts builder]
  (reduce
    (fn [s stmt]
      (build-stmt s stmt builder))
    state
    stmts))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn build-cfg
  "从 IRStmt 节点构造 CFG。

   root    —— IRStmt 节点，通常是 function/method 的 body（:block）。
   builder —— ICFGBuilder 实例。

   root 的 decon 必须返回 {:kind :block ...}——内部通过 decon 拿
   规范形式。root 的 ret 是函数返回值——未终结时回退到 :return nil。"
  [root builder]
  (let [root-form (decon builder root)]
    (when-not (= :block (:kind root-form))
      (throw (ex-info "build-cfg: root must decon to :block"
                      {:root-kind (:kind root-form)
                       :root      root})))
    (let [[entry-id state] (fresh-id (make-state))
          st-entry-b            (with-current state entry-id (:origin root-form))
          st-entry-c            (build-block st-entry-b root-form builder true)
          st-entry-d            (if (:current-id st-entry-c)
                                  (close-with-return st-entry-c nil)
                                  st-entry-c)
          {:keys [blocks loop-heads]} st-entry-d]
      (cfg/make-cfg entry-id
                    (vals blocks)
                    loop-heads))))
(ns top.kzre.homunculus.core.cfg.build
  "IRStmt → CFG 构造。

   ── IR 不变量 ──────────────────────────────────────

   IRStmt 的 block `:ret` 保证是简单表达式：
   ▸ :variable    —— 变量引用
   ▸ :literal     —— 字面量
   ▸ :call        —— 函数调用
   ▸ :member-access / :aget / :convert —— 无控制流的派生表达式

   控制流（:if / :while / :throw）只在 `:stmts` 里出现。
   statementize 负责这个不变量——block 的 ret 永远不会是
   `(if c a b)` 这种形态。

   builder 可以依赖这个不变量：ret 不需要递归展开，直接按
   return? 处理。

   ── 协议不依赖具体 AST ──────────────────────────────

   decon 返回规范形式：
     ▸ {:kind :if    :test expr :then block-form :else block-form|nil}
     ▸ {:kind :while :test expr :body block-form}
     ▸ {:kind :throw :expr expr}
     ▸ {:kind :block :stmts [...] :ret expr|nil}
     ▸ {:kind :stmt}

   block-form 是 {:kind :block :stmts [...] :ret expr|nil}——
   与任何具体前端 AST 无关。"

  (:require
    [top.kzre.homunculus.core.cfg.model :as cfg]))

;; ═══════════════════════════════════════════
;; 协议
;; ═══════════════════════════════════════════

(defprotocol ICFGBuilder
  (decon
    [_ node]
    "解构节点为规范形式之一：

     ▸ {:kind :if    :test expr :then block-form :else block-form|nil}
     ▸ {:kind :while :test expr :body block-form}
     ▸ {:kind :throw :expr expr}
     ▸ {:kind :block :stmts [stmt...] :ret expr|nil}
     ▸ {:kind :stmt}

     block-form 是 {:kind :block :stmts [...] :ret expr|nil}。
     :ret 保证是简单表达式（不变量由前端保证）。"))

;; ═══════════════════════════════════════════
;; 内部状态
;; ═══════════════════════════════════════════

(defn- fresh-id [state]
  [(:next-id state) (update state :next-id inc)])

(defn- with-current [state id origin]
  (assoc state :current-id id :current-stmts [] :current-origin origin))

(defn- append-stmt [state stmt]
  (update state :current-stmts conj stmt))

(defn- commit-with-term [state term]
  (if (nil? (:current-id state))
    state
    (let [id    (:current-id state)
          block (cfg/make-block id (:current-stmts state) term (:current-origin state))]
      (-> state
          (update :blocks assoc id block)
          (assoc :current-id nil :current-stmts [])))))

(defn- close-with-jump [state target]
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

(declare build-stmts build-stmt)

(defn- build-block
  "处理规范 block 形式 {:kind :block :stmts [...] :ret expr|nil}。

   ret 是简单表达式——按 return? 处理，不递归：
   ▸ return?=true  → :return ret（函数返回）
   ▸ return?=false → append ret（分支值，如 if 的 then/else ret）"
  [state {:keys [stmts ret]} builder return?]
  (let [state (build-stmts state stmts builder)]
    (cond
      (nil? (:current-id state)) state    ; stmts 已终结（throw）
      (nil? ret)                 state    ; 无 ret
      return?                    (close-with-return state ret)
      :else                      (append-stmt state ret))))

(defn- handle-if
  [state d builder]
  (let [{:keys [test then else]} d
        parent-origin (:current-origin state)
        has-else?     (some? else)

        [then-id  state] (fresh-id state)
        [else-id  state] (if has-else? (fresh-id state) [nil state])
        [after-id state] (fresh-id state)

        state (commit-with-term
                state
                (cfg/->Branch test then-id (or else-id after-id)))

        state (with-current state then-id then)
        state (build-block state then builder false)
        state (close-with-jump state after-id)

        state (if has-else?
                (let [state (with-current state else-id else)
                      state (build-block state else builder false)]
                  (close-with-jump state after-id))
                state)

        state (with-current state after-id parent-origin)]
    state))

(defn- handle-while
  [state d builder]
  (let [{:keys [test body]} d
        parent-origin (:current-origin state)

        [head-id state] (fresh-id state)
        [body-id state] (fresh-id state)
        [exit-id state] (fresh-id state)

        state (close-with-jump state head-id)

        state (with-current state head-id d)
        state (commit-with-term state (cfg/->Branch test body-id exit-id))

        state (with-current state body-id body)
        state (build-block state body builder false)
        state (close-with-jump state head-id)    ; ← 回边

        state (with-current state exit-id parent-origin)]
    state))

(defn- handle-throw
  [state d _builder]
  (commit-with-term state (cfg/->Throw (:expr d))))

(defn- build-stmt
  [state stmt builder]
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
  [state stmts builder]
  (reduce (fn [s stmt] (build-stmt s stmt builder)) state stmts))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn build-cfg
  "从规范 block 形式构造 CFG。

   root —— 函数体：{:kind :block :stmts [...] :ret expr|nil}
           root 的 ret 是函数返回值。

   builder —— ICFGBuilder 实例。"
  [root builder]
  (let [[entry-id state] (fresh-id {:blocks {} :next-id 0})
        state            (with-current state entry-id nil)
        state            (build-block state root builder true)
        state            (if (:current-id state)
                           (close-with-return state nil)
                           state)]
    (cfg/make-cfg entry-id (vals (:blocks state)))))
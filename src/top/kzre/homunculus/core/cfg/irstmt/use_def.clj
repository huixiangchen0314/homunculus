(ns top.kzre.homunculus.core.cfg.irstmt.use-def
  "IRStmt 的 IVarAccess 实现。

   ── IRStmt 的节点形态 ───────────────────────────────

   variable     (:name sym)            —— 读变量
   literal      (:val ...)             —— 无变量
   assign       (:var lhs :val rhs)    —— lhs 是 def（简单变量）
                                          lhs 是复合左值时内部有 uses
   var-decl     (:name sym :val init)  —— name 是 def，init 的 uses
   aset         (:target arr :idx i :val v) —— target 是 use+def
   aget         (:target arr :idx i)   —— 两个 uses
   call         (:fn f :args [...])    —— 都是 uses
   member-access (:target obj :member m) —— target 是 use
   convert      (:expr e)              —— expr 的 uses
   vector       (:items [...])         —— 都是 uses

   ── 终结指令 ────────────────────────────────────────

   Jump        无字段
   Branch      (:test expr :then id :else id)
   Return      (:value expr)
   Throw       (:value expr)

   ── aset 的 target 处理 ────────────────────────────

   简单变量 target（常见）：:name 在 uses 和 defs 里都出现——
   uses 用旧版本替换（读状态），defs 用新版本替换（写状态）。
   两步之间 target 名字从 arr → arr_old → arr_new。

   复合左值 target（罕见）：只当 use 处理（读内部变量），
   defs 返回 #{}——不产生新版本（保守处理）。"
  (:require
    [clojure.walk :as walk]
    [top.kzre.homunculus.core.cfg.analysis.use-def :as use-def]
    [top.kzre.homunculus.core.irstmt.ast :as ast]
    [top.kzre.homunculus.core.cfg.protocol :as cfg]))

;; ═══════════════════════════════════════════
;; 内部辅助
;; ═══════════════════════════════════════════

(defn- variable-node?
  [x]
  (and (ast/irstmt? x)
       (= :variable (ast/kind x))))

(defn- replace-expr-vars
  "通用表达式遍历——替换所有 :variable 节点的 :name。
   用于 expr-replace-uses 的递归。"
  [expr subst]
  (walk/postwalk
    (fn [x]
      (if (variable-node? x)
        (if-let [new-name (get subst (:name x))]
          (assoc x :name new-name)
          x)
        x))
    expr))

(defn- expr-uses*
  "IRStmt 表达式的 uses——walk 遍历所有 :variable。"
  [expr]
  (let [result (atom #{})]
    (walk/postwalk
      (fn [x]
        (when (variable-node? x)
          (swap! result conj (:name x)))
        x)
      expr)
    @result))

(defn- simple-variable?
  "是否是简单变量节点（非复合左值）。
  前端是 ClojureDSL 一定是非复合左值"
  [x]
  (and (variable-node? x) true))

;; ═══════════════════════════════════════════
;; IVarAccess 实现
;; ═══════════════════════════════════════════

(defrecord IRStmtVarAccess []
  use-def/IVarAccess

  ;; ── 表达式级 ──
  (expr-uses [_ expr]
    (expr-uses* expr))

  (expr-replace-uses [_ expr subst]
    (replace-expr-vars expr subst))

  ;; ── 语句级 ──
  (stmt-uses [_ stmt]
    (case (ast/kind stmt)
      :assign
      (let [var (:var stmt)
            val (:val stmt)]
        (if (simple-variable? var)
          ;; 简单变量——:var 是 def，不算 use
          (expr-uses* val)
          ;; 复合左值——:var 里有读 target 的 uses
          (into (expr-uses* var) (expr-uses* val))))

      :var-decl
      ;; :name 是 def，:val 的 uses
      (if-let [init (:val stmt)]
        (expr-uses* init)
        #{})

      :aset
      ;; target 是 use（读旧状态）+ idx + val 的 uses
      (let [target (:target stmt)
            idx    (:idx stmt)
            val    (:val stmt)]
        (into (if (simple-variable? target)
                #{}
                (expr-uses* target))
              (into (expr-uses* idx)
                    (expr-uses* val))))

      ;; 其他——整条语句当表达式处理
      (expr-uses* stmt)))

  (stmt-defs [_ stmt]
    (case (ast/kind stmt)
      :assign
      (let [var (:var stmt)]
        (if (simple-variable? var)
          #{(:name var)}
          #{}))

      :var-decl
      #{(:name stmt)}

      :aset
      (let [target (:target stmt)]
        (if (simple-variable? target)
          #{(:name target)}
          #{}))

      #{}))

  (stmt-replace-uses [_ stmt subst]
    (case (ast/kind stmt)
      :assign
      (let [var (:var stmt)
            val (:val stmt)]
        (if (simple-variable? var)
          ;; 简单变量——只替换 :val 里的 uses（:var 是 def，留给 defs）
          (assoc stmt :val (replace-expr-vars val subst))
          ;; 复合左值——:var 里也有 uses
          (-> stmt
              (assoc :var (replace-expr-vars var subst))
              (assoc :val (replace-expr-vars val subst)))))

      :var-decl
      ;; 只替换 :val，:name 留给 defs
      (if-let [init (:val stmt)]
        (assoc stmt :val (replace-expr-vars init subst))
        stmt)

      :aset
      (let [target (:target stmt)
            idx    (:idx stmt)
            val    (:val stmt)]
        ;; target 作为 use 替换成旧版本（简单变量）；复合左值递归
        (-> stmt
            (assoc :target (replace-expr-vars target subst))
            (assoc :idx    (replace-expr-vars idx subst))
            (assoc :val    (replace-expr-vars val subst))))

      ;; 其他——整条语句替换
      (replace-expr-vars stmt subst)))

  (stmt-replace-defs [_ stmt subst]
    (case (ast/kind stmt)
      :assign
      (let [var (:var stmt)]
        (if (simple-variable? var)
          (if-let [new-name (get subst (:name var))]
            (assoc stmt :var (assoc var :name new-name))
            stmt)
          ;; 复合左值——defs 为空，不替换
          stmt))

      :var-decl
      (if-let [new-name (get subst (:name stmt))]
        (assoc stmt :name new-name)
        stmt)

      :aset
      (let [target (:target stmt)]
        (if (simple-variable? target)
          (if-let [new-name (get subst (:name target))]
            (assoc stmt :target (assoc target :name new-name))
            stmt)
          stmt))

      stmt))

  ;; ── 终结指令级 ──
  (term-uses [_ term]
    (case (cfg/term-kind term)
      :jump   #{}
      :branch (expr-uses* (:test term))
      :return (if-let [v (:value term)] (expr-uses* v) #{})
      :throw  (if-let [v (:value term)] (expr-uses* v) #{})
      #{}))

  (term-replace-uses [_ term subst]
    (case (cfg/term-kind term)
      :jump   term
      :branch (assoc term :test (replace-expr-vars (:test term) subst))
      :return (if-let [v (:value term)]
                (assoc term :value (replace-expr-vars v subst))
                term)
      :throw  (if-let [v (:value term)]
                (assoc term :value (replace-expr-vars v subst))
                term)
      term))
  (make-variable-ref [_ sym]
    (ast/->Variable sym {} nil)))

;; ═══════════════════════════════════════════
;; 工厂
;; ═══════════════════════════════════════════

(defonce ^:private adapter* (->IRStmtVarAccess))

(defn adapter []
  adapter*)
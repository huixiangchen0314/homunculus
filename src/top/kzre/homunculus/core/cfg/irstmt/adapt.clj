(ns top.kzre.homunculus.core.cfg.irstmt.adapt
  "IRStmt 的 IAdapter 实现。

   ── 变量身份 ──

   ▸ assign / var-decl 定义变量——stmt-defs 有值
   ▸ aset / call 不定义变量——纯副作用，stmt-defs 为 ∅

   ── Clojure DSL 的赋值约定 ──

   assign 的 :var 一定是 variable 节点——Clojure DSL 没有
   C 的 a = b = c 或 arr[i] = v 形态。左值永远是简单变量。
   var-decl 的 :name 是符号定义；:val 是唯一的表达式子节点。

   两条约定让 assign / var-decl 无需 variable-node? 判断：
   ▸ assign 的 use 只有 :val
   ▸ var-decl 的 use 只有 :val（:name 是符号，不是变量节点）
   ▸ aset / call 走一般路径——ast/children 遍历所有子表达式

   ── 递归遍历 ──

   expr-uses* / replace-expr-vars 走 ast/children 和
   ast/rreduce-children——不依赖 clojure.walk。IRStmt 的
   自定义子节点结构只有协议知道。"
  (:require
   [top.kzre.homunculus.core.cfg.adapt :as adapt]
   [top.kzre.homunculus.core.cfg.protocol :as proto]
   [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]
   [top.kzre.homunculus.core.irstmt.ast :as ast]
   [top.kzre.homunculus.core.irstmt.pass.dce.api :as dce]
   [top.kzre.homunculus.core.symbol :as sym]
   [top.kzre.homunculus.internal.protocol :as ip]))

;; ═══════════════════════════════════════════
;; 内部辅助
;; ═══════════════════════════════════════════

(defn- variable-node? [x]
  (and (ast/irstmt? x) (= :variable (ast/kind x))))

(defn- expr-uses*
  "表达式 expr 里被读取的变量名集合。

   用 ast/children 递归——IRStmt 的子节点结构由协议定义。"
  [expr]
  (cond
    (nil? expr)             #{}
    (variable-node? expr)   #{(:name expr)}
    (ast/irstmt? expr)    (reduce (fn [acc child]
                                      (into acc (expr-uses* child)))
                                    #{}
                                    (ast/children expr))
    :else                   #{}))

(defn- replace-expr-vars
  "按 subst 替换表达式里的 variable 节点名。

   用 ast/rreduce-children 深度替换——保证输出仍是 IRStmt record，
   不会降级为普通 map。"
  [expr subst]
  (cond
    (nil? expr)             nil
    (variable-node? expr)   (if-let [new-name (get subst (:name expr))]
                              (assoc expr :name new-name)
                              expr)
    (ast/irstmt? expr)    (first
                              (ast/reduce-children
                                expr
                                (fn [child _]
                                  [(replace-expr-vars child subst) nil])
                                nil))
    :else                   expr))

(defn- copy-construct?
  "node 的赋值是否涉及拷贝构造（分配 + 复制）。

   ▸ vector 类型：后端 mutable 且 copy-assign 时是拷贝构造
   ▸ record 类型：符号表里注册的 record，赋值是拷贝
   ▸ 无类型 / 其他类型：不是拷贝构造"
  [node compile-ctx]
  (if-let [t (ty/get-type node)]
    (let [backend (ip/backend compile-ctx)]
      (if (tp/copy-on-assign? backend)
        (cond
          (ty/vec-type? t)
          (and (tp/mutable-vector? backend)
               (tp/vector-copy-assign? backend))

          :else
          (boolean
            (sym/lookup-record (ip/symbol-table compile-ctx)
                               (ty/type-sym t))))
        false))
    false))

;; ═══════════════════════════════════════════
;; IAdapter 实现
;; ═══════════════════════════════════════════

(defrecord IRStmtAdapter [compile-ctx]
  adapt/IAdapter

  ;; ── 表达式级 ──
  (expr-uses [_ expr] (expr-uses* expr))
  (expr-replace-uses [_ expr subst] (replace-expr-vars expr subst))

  ;; ── 语句级 ──
  (stmt-uses [_ stmt]
    (case (ast/kind stmt)
      ;; assign 的 use 只有 :val——:var 是简单变量，被定义不算 use
      :assign   (expr-uses* (:val stmt))

      ;; var-decl / aset / call / 其他——走一般路径
      ;; var-decl 的 ast/children 只返回 :val
      ;; aset 的 target / idx / val 都是 use
      (expr-uses* stmt)))

  (stmt-defs [_ stmt]
    (case (ast/kind stmt)
      ;; assign 定义简单变量——约定 :var 一定是 variable
      :assign   #{(:name (:var stmt))}
      :var-decl #{(:name stmt)}
      ;; aset / call / 其他——不定义
      #{}))

  (stmt-replace-uses [_ stmt subst]
    (case (ast/kind stmt)
      ;; assign——只替换 :val 里的 uses
      :assign (assoc stmt :val (replace-expr-vars (:val stmt) subst))
      ;; var-decl / aset / call / 其他——深度替换所有子表达式
      (replace-expr-vars stmt subst)))

  (stmt-replace-defs [_ stmt subst]
    (case (ast/kind stmt)
      :assign
      (let [v (:var stmt)
            n (get subst (:name v))]
        (if n
          (assoc stmt :var (assoc v :name n))
          stmt))

      :var-decl
      (if-let [n (get subst (:name stmt))]
        (assoc stmt :name n)
        stmt)

      ;; aset / call 不定义——原样返回
      stmt))
  (stmt-fx? [_ stmt]
    (let [fx? (get-in stmt [:attrs :fx?])]
      (if (boolean? fx?)
        fx?
        (let [fxed (first (dce/elim-node stmt (dce/make-env compile-ctx)))]
          (dce/fx? fxed)))))
  (assign-stmt? [_ stmt]
    (= :assign (ast/kind stmt)))

  ;; ── 终结指令级 ──
  (term-uses [_ term]
    (case (proto/term-kind term)
      :jump   #{}
      :branch (expr-uses* (:test term))
      :return (if-let [v (:value term)] (expr-uses* v) #{})
      :throw  (if-let [v (:value term)] (expr-uses* v) #{})
      #{}))

  (term-replace-uses [_ term subst]
    (case (proto/term-kind term)
      :jump   term
      :branch (assoc term :test (replace-expr-vars (:test term) subst))
      :return (if-let [v (:value term)]
                (assoc term :value (replace-expr-vars v subst))
                term)
      :throw  (if-let [v (:value term)]
                (assoc term :value (replace-expr-vars v subst))
                term)
      term)
    )
  (simple-move?
    [_ stmt]
    (and (= :assign (ast/kind stmt))
         (if-let [rhs (:val stmt)]
           (= :variable (ast/kind rhs))
           false)))
  ;; ── 节点构造——直接复制 attrs ──
  (make-var-ref [_ template sym]
    (when-not template
      (throw (ex-info "make-var-ref: template cannot be nil" {:sym sym})))
    (ast/->Variable sym (:attrs template) (ast/node-meta template)))

  (make-move [_ var-sym value]
    (ast/->Assign
      (ast/->Variable
        var-sym (:attrs value) (ast/node-meta value))
      value (:attrs value) (ast/node-meta value)))

  ;; ── 节点构造（bake 用）──
  (make-block [_ stmts ret]
    (ast/->Block (vec stmts)
                 ret
                 {}
                 {}))

  (make-if [_ test then-block else-block ]
    (ast/->If test
              then-block
              else-block
              {}
              {}))

  (make-throw [_ value ]
    (ast/->Throw value
                 {}
                 {}))

  (make-while [_ test body-block ]
    (ast/->While test body-block
                 {}
                 {})))

;; ═══════════════════════════════════════════
;; 单例
;; ═══════════════════════════════════════════

(defn make-adapter [compile-ctx]
  (->IRStmtAdapter compile-ctx))
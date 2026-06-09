(ns top.kzre.homunculus.backends.hlsl.ir3-hlsl
  (:require [top.kzre.homunculus.core.ir2 :as ir2]
            [clojure.spec.alpha :as s]))

;; ── HLSL 类型关键字 ──────────────────────────────
(def hlsl-types
  {:float  "float"
   :int    "int"
   :bool   "bool"
   :float2 "float2"
   :float3 "float3"
   :float4 "float4"
   :float4x4 "float4x4"})

(defn- make-hlsl-type [base dim]
  (keyword (str (name base) (when (> dim 1) dim))))

;; ── HLSL 中间表示记录 ────────────────────────────
(defrecord HlslLiteral [value type])
(defrecord HlslVarRef [name type])
(defrecord HlslBinaryOp [op left right type])
(defrecord HlslCall [func args type])
(defrecord HlslConstructor [base-type elements])
(defrecord HlslSwizzle [base swizzle type])
(defrecord HlslMemberAccess [base member type])
(defrecord HlslIfExpr [test then else type])
(defrecord HlslLetExpr [bindings body type])

;; 语句
(defrecord HlslExprStmt [expr])
(defrecord HlslIfStmt [test then else])
(defrecord HlslReturnStmt [expr])
(defrecord HlslLoopStmt [bindings body])
(defrecord HlslVarDecl [name type init])
(defrecord HlslFunction [name params return-type body])

;; 程序
(defrecord HlslProgram [uniforms functions])

;; ── 类型推导 ────────────────────────────────────
(defn- infer-type
  [node-or-val]
  (cond
    (instance? HlslLiteral node-or-val) (:type node-or-val)
    (and (map? node-or-val) (= (::ir2/kind node-or-val) :literal))
    (let [v (:val node-or-val)]
      (cond
        (number? v) (if (or (float? v) (ratio? v) (double? v)) :float :int)
        (true? v) :bool
        (false? v) :bool
        :else :float))
    (instance? HlslVarRef node-or-val) (:type node-or-val)
    (instance? HlslBinaryOp node-or-val) (:type node-or-val)
    (instance? HlslConstructor node-or-val)
    (make-hlsl-type (:base-type node-or-val) (count (:elements node-or-val)))
    :else :float))

;; ── IR2 → HLSL 表达式 ──────────────────────────
(declare ir2->ir3-hlsl-stmt)
(defn ir2->ir3-hlsl-expr
  [ir2-vec env]
  (let [node (first ir2-vec)
        kind (::ir2/kind node)]
    (case kind
      :literal
      (let [v (:val node)
            t (infer-type node)]
        (->HlslLiteral v t))

      :var
      (let [sym (:name node)
            t (get-in env [:vars sym] :float)]
        (->HlslVarRef sym t))

      :prim
      (let [op (:op node)
            args (map #(ir2->ir3-hlsl-expr % env) (rest ir2-vec))]
        (case op
          (:add :sub :mul :div :mod :rem :eq :neq :lt :lte :gt :gte :and :or)
          (let [op-name (name op)
                op-map {"add" "+" "sub" "-" "mul" "*" "div" "/"
                        "mod" "%" "rem" "%"
                        "eq" "==" "neq" "!=" "lt" "<" "lte" "<="
                        "gt" ">" "gte" ">="
                        "and" "&&" "or" "||"}
                hlsl-op (op-map op-name)]
            (->HlslBinaryOp (keyword hlsl-op) (first args) (second args)
                            (if (contains? #{"==" "!=" "<" "<=" ">" ">="} hlsl-op)
                              :bool
                              (infer-type (first args)))))

          :not (->HlslBinaryOp :! (first args) nil :bool)
          :print nil
          :first (->HlslSwizzle (first args) "x" (infer-type (first args)))
          :second (->HlslSwizzle (first args) "y" (infer-type (first args)))
          :nth (let [coll (first args) idx (second args)]
                 (if (instance? HlslLiteral idx)
                   (->HlslSwizzle coll (str (:value idx)) :float)
                   (->HlslMemberAccess coll (str "[" (pr-str idx) "]") :float)))
          :count nil
          :get (->HlslMemberAccess (first args) (second args) :float)
          ;; 默认：返回第一个参数
          (first args)))

      :call
      (let [func-expr (ir2->ir3-hlsl-expr (second ir2-vec) env)
            args (map #(ir2->ir3-hlsl-expr % env) (nthrest ir2-vec 2))]
        (if (instance? HlslVarRef func-expr)
          (->HlslCall (:name func-expr) args (infer-type (last args)))
          nil))

      :if
      (let [test (ir2->ir3-hlsl-expr (second ir2-vec) env)
            then (ir2->ir3-hlsl-expr (nth ir2-vec 2) env)
            else (when (>= (count ir2-vec) 4)
                   (ir2->ir3-hlsl-expr (nth ir2-vec 3) env))]
        (->HlslIfExpr test then else (infer-type then)))

      :do
      (let [exprs (map #(ir2->ir3-hlsl-expr % env) (rest ir2-vec))
            last-expr (last exprs)]
        (if (seq exprs)
          (->HlslLetExpr [] last-expr (infer-type last-expr))
          nil))

      :let
      (let [bindings-count (:bindings-count node)
            all-children (rest ir2-vec)
            bind-irs (take bindings-count all-children)
            body-irs (drop bindings-count all-children)
            syms (map #(ir2->ir3-hlsl-expr % env) (take-nth 2 bind-irs))
            vals (map #(ir2->ir3-hlsl-expr % env) (take-nth 2 (rest bind-irs)))
            sym-names (map :name syms)
            new-env (reduce (fn [e [n v]]
                              (assoc-in e [:vars n] (infer-type v)))
                            env
                            (map vector sym-names vals))
            body-expr (ir2->ir3-hlsl-expr (first body-irs) new-env)]
        (->HlslLetExpr (map vector sym-names vals) body-expr (infer-type body-expr)))

      :fn
      (let [params (::ir2/params node)
            fn-name (::ir2/fn-name node)
            body-irs (rest ir2-vec)
            param-types (repeat (count params) :float)
            env' (reduce (fn [e [sym t]]
                           (assoc-in e [:vars (name sym)] t))
                         env
                         (map vector params param-types))
            ;; 从最后一个 body 表达式推导返回类型
            return-type (if (seq body-irs)
                          (:type (ir2->ir3-hlsl-expr (last body-irs) env'))
                          :float)
            body-stmts (mapcat #(ir2->ir3-hlsl-stmt % env') body-irs)]
        [(->HlslFunction (name fn-name)
                         (map name params)
                         return-type
                         body-stmts)])

      :vector
      (let [elems (map #(ir2->ir3-hlsl-expr % env) (rest ir2-vec))]
        (->HlslConstructor :float elems))

      nil)))

;; ── IR2 → HLSL 语句 ────────────────────────────
(defn ir2->ir3-hlsl-stmt
  [ir2-vec env]
  (let [node (first ir2-vec)
        kind (::ir2/kind node)]
    (case kind
      :def
      (let [name-expr (ir2->ir3-hlsl-expr (second ir2-vec) env)
            val-ir (when (>= (count ir2-vec) 3) (nth ir2-vec 2))]
        (if (and val-ir (= (::ir2/kind (first val-ir)) :fn))
          ;; 值是函数 → 直接作为函数语句处理
          (ir2->ir3-hlsl-stmt val-ir env)
          ;; 普通值 → 变量声明
          (let [val-expr (when val-ir (ir2->ir3-hlsl-expr val-ir env))
                t (if val-expr (infer-type val-expr) :float)]
            [(->HlslVarDecl (:name name-expr) t val-expr)])))

      :fn
      (let [params (::ir2/params node)
            fn-name (or (::ir2/fn-name node) 'anonymous)
            all-children (rest ir2-vec)
            param-count (count params)
            body-irs (drop param-count all-children)   ;; 参数之后全部作为 body
            param-types (repeat param-count :float)
            env' (reduce (fn [e [sym t]]
                           (assoc-in e [:vars (name sym)] t))
                         env
                         (map vector params param-types))
            return-type (if (seq body-irs)
                          (:type (ir2->ir3-hlsl-expr (last body-irs) env'))
                          :float)
            body-stmts (mapcat #(ir2->ir3-hlsl-stmt % env') body-irs)]
        [(->HlslFunction (name fn-name)
                         (map name params)
                         return-type
                         body-stmts)])

      :do
      (mapcat #(ir2->ir3-hlsl-stmt % env) (rest ir2-vec))

      :if
      (let [test (ir2->ir3-hlsl-expr (second ir2-vec) env)
            then-stmts (ir2->ir3-hlsl-stmt (nth ir2-vec 2) env)
            else-stmts (when (>= (count ir2-vec) 4)
                         (ir2->ir3-hlsl-stmt (nth ir2-vec 3) env))]
        [(->HlslIfStmt test then-stmts else-stmts)])

      :loop
      (let [bindings-count (:bindings-count node)
            all-children (rest ir2-vec)
            bind-irs (take bindings-count all-children)
            body-irs (drop bindings-count all-children)
            syms (map #(ir2->ir3-hlsl-expr % env) (take-nth 2 bind-irs))
            vals (map #(ir2->ir3-hlsl-expr % env) (take-nth 2 (rest bind-irs)))
            sym-names (map :name syms)
            env' (reduce (fn [e [n v]]
                           (assoc-in e [:vars n] (infer-type v)))
                         env
                         (map vector sym-names vals))
            body-stmts (mapcat #(ir2->ir3-hlsl-stmt % env') body-irs)]
        [(->HlslLoopStmt (map vector sym-names vals) body-stmts)])

      :recur []

      (if-let [expr (ir2->ir3-hlsl-expr ir2-vec env)]
        [(->HlslExprStmt expr)]
        []))))

;; ── 程序级转换 ─────────────────────────────────
(defn ir2->ir3-hlsl [ir2-vecs]
  (let [initial-env {:vars {}}
        all-stmts (mapcat #(ir2->ir3-hlsl-stmt % initial-env) ir2-vecs)
        uniforms (vec (filter #(instance? HlslVarDecl %) all-stmts))
        functions (vec (filter #(instance? HlslFunction %) all-stmts))]
    (->HlslProgram uniforms functions)))
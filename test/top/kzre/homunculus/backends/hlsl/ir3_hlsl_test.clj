(ns top.kzre.homunculus.backends.hlsl.ir3-hlsl-test
  (:require
    [clojure.test :refer :all]
    [top.kzre.homunculus.backends.hlsl.ir3-hlsl :as h]
    [top.kzre.homunculus.core.ir2 :as ir2])
  (:import (top.kzre.homunculus.backends.hlsl.ir3_hlsl HlslBinaryOp HlslCall HlslConstructor HlslFunction HlslIfExpr HlslIfStmt HlslLetExpr HlslLiteral HlslLoopStmt HlslProgram HlslSwizzle HlslVarDecl HlslVarRef)))

;; ── 辅助函数 ──────────────────────────────────────
(def ^:private kind-key :top.kzre.homunculus.core.ir2/kind)
(def ^:private params-key :top.kzre.homunculus.core.ir2/params)

(defn- make-node [kind & kvs]
  (if (seq kvs)
    (apply assoc {kind-key kind} kvs)
    {kind-key kind}))

(defn- lit [v] [(make-node :literal :val v :type (type v))])
(defn- var-node [sym] [(make-node :var :name sym)])

(defn- prim [op & args]
  (vec (cons (make-node :prim :op op) args)))

(defn- call [fn-ir & arg-irs]
  (vec (cons (make-node :call) (cons fn-ir arg-irs))))

(defn- if-node [test then else]
  (vec (filter some? (cons (make-node :if) [test then else]))))

(defn- do-node [& exprs]
  (vec (cons (make-node :do) exprs)))

(defn- let-node [bind-pairs & body]
  (vec (cons (assoc (make-node :let) :bindings-count (count bind-pairs))
             (concat bind-pairs body))))

(defn- fn-node [name params & body]
  (let [base (assoc (make-node :fn) params-key params)]
    (vec (cons (if name (assoc base ::ir2/fn-name name) base)
               body))))

(defn- def-node [name & [val]]
  (vec (filter some? (cons (make-node :def) [name val]))))

(defn- vec-node [& items]
  (vec (cons (make-node :vector) items)))

(defn- loop-node [bind-pairs & body]
  (vec (cons (assoc (make-node :loop) :bindings-count (count bind-pairs))
             (concat bind-pairs body))))

;; ── 测试字面量 ──────────────────────────────────
(deftest literal-test
  (testing "整数字面量"
    (let [result (h/ir2->ir3-hlsl-expr (lit 42) {:vars {}})]
      (is (instance? HlslLiteral result))
      (is (= 42 (:value result)))
      (is (= :int (:type result)))))
  (testing "浮点字面量"
    (let [result (h/ir2->ir3-hlsl-expr (lit 3.14) {:vars {}})]
      (is (instance? HlslLiteral result))
      (is (= 3.14 (:value result)))
      (is (= :float (:type result)))))
  (testing "布尔字面量"
    (let [result (h/ir2->ir3-hlsl-expr (lit true) {:vars {}})]
      (is (instance? HlslLiteral result))
      (is (= true (:value result)))
      (is (= :bool (:type result)))))
  (testing "nil → 默认 float（暂定）"
    (let [result (h/ir2->ir3-hlsl-expr (lit nil) {:vars {}})]
      (is (instance? HlslLiteral result))
      (is (= :float (:type result))))))

;; ── 测试变量引用 ──────────────────────────────
(deftest var-test
  (testing "已知类型的变量"
    (let [env {:vars {'x :float}}
          result (h/ir2->ir3-hlsl-expr (var-node 'x) env)]
      (is (instance? HlslVarRef result))
      (is (= 'x (:name result)))
      (is (= :float (:type result)))))
  (testing "未声明变量默认 float"
    (let [result (h/ir2->ir3-hlsl-expr (var-node 'y) {:vars {}})]
      (is (= :float (:type result))))))

;; ── 测试原语操作 ──────────────────────────────
(deftest prim-test
  (testing "加法"
    (let [add-ir (prim :add (lit 1) (lit 2))
          result (h/ir2->ir3-hlsl-expr add-ir {:vars {}})]
      (is (instance? HlslBinaryOp result))
      (is (= :+ (:op result)))
      (is (= :int (:type result)))))
  (testing "比较操作返回 bool"
    (let [result (h/ir2->ir3-hlsl-expr (prim :lt (lit 1) (lit 2)) {:vars {}})]
      (is (instance? HlslBinaryOp result))
      (is (= :< (:op result)))
      (is (= :bool (:type result)))))
  (testing "not 操作"
    (let [result (h/ir2->ir3-hlsl-expr (prim :not (lit true)) {:vars {}})]
      (is (instance? HlslBinaryOp result))
      (is (= :! (:op result)))
      (is (= :bool (:type result)))))
  (testing "first 操作 → swizzle x"
    (let [env {:vars {'v :float3}}
          result (h/ir2->ir3-hlsl-expr (prim :first (var-node 'v)) env)]
      (is (instance? HlslSwizzle result))
      (is (= "x" (:swizzle result)))))
  (testing "nth 常量索引 → swizzle"
    (let [result (h/ir2->ir3-hlsl-expr (prim :nth (var-node 'v) (lit 2)) {:vars {'v :float4}})]
      (is (instance? HlslSwizzle result))
      (is (= "2" (:swizzle result))))))

;; ── 测试函数调用 ──────────────────────────────
(deftest call-test
  (testing "简单调用"
    (let [call-ir (call (var-node 'f) (lit 1) (lit 2))
          result (h/ir2->ir3-hlsl-expr call-ir {:vars {}})]
      (is (instance? HlslCall result))
      (is (= 'f (:func result)))
      (is (= 2 (count (:args result)))))))

;; ── 测试 if 表达式 ────────────────────────────
(deftest if-expr-test
  (testing "if 作为表达式（三元）"
    (let [test (prim :gt (var-node 'x) (lit 0))
          then (lit 1)
          else (lit 0)
          if-ir (if-node test then else)
          result (h/ir2->ir3-hlsl-expr if-ir {:vars {}})]
      (is (instance? HlslIfExpr result))
      (is (instance? HlslBinaryOp (:test result)))
      (is (= :int (:type result)))))
  (testing "无 else 分支"
    (let [if-ir (if-node (prim :eq (var-node 'y) (lit 0)) (lit 1) nil)
          result (h/ir2->ir3-hlsl-expr if-ir {:vars {}})]
      (is (instance? HlslIfExpr result))
      (is (nil? (:else result))))))

;; ── 测试 let 表达式 ──────────────────────────
(deftest let-expr-test
  (testing "简单 let 绑定"
    (let [bind-pairs [(var-node 'x) (lit 10)]
          body (prim :add (var-node 'x) (lit 1))
          let-ir (let-node bind-pairs body)
          result (h/ir2->ir3-hlsl-expr let-ir {:vars {}})]
      (is (instance? HlslLetExpr result))
      (is (= 1 (count (:bindings result))))
      (let [[[name init]] (:bindings result)]
        (is (= 'x name))
        (is (instance? HlslLiteral init)))
      (is (instance? HlslBinaryOp (:body result)))
      (is (= :int (:type result))))))

;; ── 测试向量构造 ──────────────────────────────
(deftest vector-test
  (testing "向量构造"
    (let [vec-ir (vec-node (lit 1) (lit 2) (lit 3))
          result (h/ir2->ir3-hlsl-expr vec-ir {:vars {}})]
      (is (instance? HlslConstructor result))
      (is (= :float (:base-type result)))
      (is (= 3 (count (:elements result)))))))

;; ── 测试语句：def → 变量声明 ──────────────────
(deftest def-stmt-test
  (testing "顶层变量声明"
    (let [def-ir (def-node (var-node 'radius) (lit 5.0))
          stmts (h/ir2->ir3-hlsl-stmt def-ir {:vars {}})]
      (is (= 1 (count stmts)))
      (let [decl (first stmts)]
        (is (instance? HlslVarDecl decl))
        (is (= 'radius (:name decl)))
        (is (= :float (:type decl)))
        (is (instance? HlslLiteral (:init decl)))))))

;; ── 测试函数定义 ──────────────────────────────
(deftest fn-stmt-test
  (testing "具名函数定义"
    (let [fn-ir (fn-node 'add ['a 'b] (prim :add (var-node 'a) (var-node 'b)))
          stmts (h/ir2->ir3-hlsl-stmt fn-ir {:vars {}})]
      (is (= 1 (count stmts)))
      (let [func (first stmts)]
        (is (instance? HlslFunction func))
        (is (= "add" (:name func)))
        (is (= '("a" "b") (:params func)))
        (is (= :float (:return-type func)))   ; 改为 :float
        (is (seq (:body func)))))))

;; ── 测试 if 语句 ────────────────────────────
(deftest if-stmt-test
  (testing "if 语句"
    (let [then (do-node (prim :add (var-node 'x) (lit 1)))
          else (do-node (prim :sub (var-node 'x) (lit 1)))
          if-ir (if-node (prim :gt (var-node 'x) (lit 0)) then else)
          stmts (h/ir2->ir3-hlsl-stmt if-ir {:vars {'x :float}})]
      (is (= 1 (count stmts)))
      (let [if-stmt (first stmts)]
        (is (instance? HlslIfStmt if-stmt))
        (is (instance? HlslBinaryOp (:test if-stmt)))
        (is (seq (:then if-stmt)))
        (is (seq (:else if-stmt)))))))

;; ── 测试 loop 语句 ──────────────────────────
(deftest loop-stmt-test
  (testing "loop 语句"
    (let [bind-pairs [(var-node 'i) (lit 0)]
          body (prim :add (var-node 'i) (lit 1))
          loop-ir (loop-node bind-pairs body)
          stmts (h/ir2->ir3-hlsl-stmt loop-ir {:vars {}})]
      (is (= 1 (count stmts)))
      (let [loop-stmt (first stmts)]
        (is (instance? HlslLoopStmt loop-stmt))
        (is (= 1 (count (:bindings loop-stmt))))
        (is (seq (:body loop-stmt)))))))

;; ── 测试程序级转换 ──────────────────────────
(deftest program-test
  (testing "简单程序"
    (let [prog [(def-node (var-node 'scale) (lit 2.0))
                (fn-node 'applyScale ['x] (prim :mul (var-node 'x) (var-node 'scale)))]
          result (h/ir2->ir3-hlsl prog)]
      (is (instance? HlslProgram result))
      (is (= 1 (count (:uniforms result))))
      (is (= 1 (count (:functions result)))))))
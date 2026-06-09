(ns top.kzre.homunculus.backends.hlsl.emit-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.backends.hlsl.ir3-hlsl :as ir3]
            [top.kzre.homunculus.backends.hlsl.emit :as emit])
  (:import (top.kzre.homunculus.backends.hlsl.ir3_hlsl
             HlslLiteral HlslVarRef HlslBinaryOp HlslCall
             HlslConstructor HlslSwizzle HlslMemberAccess
             HlslIfExpr HlslLetExpr
             HlslExprStmt HlslIfStmt HlslReturnStmt
             HlslLoopStmt HlslVarDecl HlslFunction HlslProgram)))

;; ── 辅助构造函数 ────────────────────────────────
(defn- lit [v t] (HlslLiteral. v t))
(defn- var-ref [sym t] (HlslVarRef. sym t))
(defn- binary [op left right t] (HlslBinaryOp. op left right t))
(defn- call [func args t] (HlslCall. func args t))
(defn- constructor [base-type elements] (HlslConstructor. base-type elements))
(defn- swizzle [base swiz t] (HlslSwizzle. base swiz t))
(defn- member [base mem t] (HlslMemberAccess. base mem t))
(defn- if-expr [test then else t] (HlslIfExpr. test then else t))

(defn- expr-stmt [e] (HlslExprStmt. e))
(defn- var-decl [name t init] (HlslVarDecl. name t init))
(defn- if-stmt [test then else] (HlslIfStmt. test then else))
(defn- return-stmt [e] (HlslReturnStmt. e))
(defn- loop-stmt [bindings body] (HlslLoopStmt. bindings body))
(defn- fn-def [name params return-type body] (HlslFunction. name params return-type body))
(defn- program [uniforms functions] (HlslProgram. uniforms functions))

;; ── 测试字面量 ──────────────────────────────────
(deftest literal-test
  (testing "整数字面量"
    (is (= "42" (emit/emit-expr (lit 42 :int)))))
  (testing "浮点字面量"
    (is (= "3.14" (emit/emit-expr (lit 3.14 :float)))))
  (testing "布尔真"
    (is (= "true" (emit/emit-expr (lit true :bool)))))
  (testing "nil 视为 false"
    (is (= "false" (emit/emit-expr (lit nil :float))))))

;; ── 测试变量引用 ──────────────────────────────
(deftest var-ref-test
  (testing "变量名直接输出"
    (is (= "position" (emit/emit-expr (var-ref 'position :float3))))))

;; ── 测试二元运算符 ────────────────────────────
(deftest binary-op-test
  (testing "加法"
    (let [expr (binary :+ (lit 1 :int) (lit 2 :int) :int)]
      (is (= "(1 + 2)" (emit/emit-expr expr)))))
  (testing "逻辑非"
    (let [expr (binary :! (lit true :bool) nil :bool)]
      (is (= "!true" (emit/emit-expr expr))))))

;; ── 测试函数调用 ──────────────────────────────
(deftest call-test
  (testing "带参数的调用"
    (let [expr (call "max" [(lit 3.0 :float) (lit 5.0 :float)] :float)]
      (is (= "max(3.0, 5.0)" (emit/emit-expr expr))))))

;; ── 测试向量/矩阵构造 ─────────────────────────
(deftest constructor-test
  (testing "float2 构造"
    (let [expr (constructor :float [(lit 1.0 :float) (lit 2.0 :float)])]
      (is (= "float2(1.0, 2.0)" (emit/emit-expr expr)))))
  (testing "标量类型强制"
    (let [expr (constructor :float [(lit 42 :int)])]
      (is (= "(float) 42" (emit/emit-expr expr))))))

;; ── 测试 Swizzle / 成员访问 ───────────────────
(deftest swizzle-test
  (testing "swizzle"
    (let [expr (swizzle (var-ref 'v :float4) "xyz" :float3)]
      (is (= "v.xyz" (emit/emit-expr expr)))))
  (testing "成员访问"
    (let [expr (member (var-ref 's :float) 'x :float)]
      (is (= "s.x" (emit/emit-expr expr))))))

;; ── 测试三元表达式 ────────────────────────────
(deftest if-expr-test
  (testing "三元有 else"
    (let [expr (if-expr (binary :> (var-ref 'a :float) (lit 0 :float) :bool)
                        (lit 1.0 :float)
                        (lit -1.0 :float)
                        :float)]
      (is (= "((a > 0) ? 1.0 : -1.0)" (emit/emit-expr expr)))))
  (testing "三元无 else"
    (let [expr (if-expr (binary :!= (var-ref 'x :float) (lit 0 :float) :bool)
                        (lit 1 :int)
                        nil
                        :int)]
      (is (= "((x != 0) ? 1 : false)" (emit/emit-expr expr))))))

;; ── 测试表达式语句 ────────────────────────────
(deftest expr-stmt-test
  (testing "表达式语句加分号"
    (let [stmt (expr-stmt (call "foo" [] :void))]
      (is (= "foo();\n" (emit/emit-stmt stmt))))))

;; ── 测试变量声明 ──────────────────────────────
(deftest var-decl-test
  (testing "带初始化的声明"
    (let [stmt (var-decl 'radius :float (lit 5.0 :float))]
      (is (= "float radius = 5.0;\n" (emit/emit-stmt stmt)))))
  (testing "无初始化的声明"
    (let [stmt (var-decl 'value :int nil)]
      (is (= "int value;\n" (emit/emit-stmt stmt))))))

;; ── 测试 if 语句 ──────────────────────────────
(deftest if-stmt-test
  (testing "if 语句带大括号"
    (let [then [(expr-stmt (call "discard" [] :void))]
          else [(expr-stmt (call "return" [(lit 0 :int)] :void))]
          stmt (if-stmt (binary :< (var-ref 'x :float) (lit 0 :float) :bool)
                        then else)]
      (is (= "if ((x < 0))\n{\n  discard();\n}\nelse\n{\n  return(0);\n}\n"
             (emit/emit-stmt stmt))))))

;; ── 测试 return 语句 ──────────────────────────
(deftest return-stmt-test
  (testing "返回表达式"
    (let [stmt (return-stmt (lit 1 :int))]
      (is (= "return 1;\n" (emit/emit-stmt stmt))))))

;; ── 测试循环语句 ──────────────────────────────
(deftest loop-stmt-test
  (testing "简单 for(;;) 循环"
    (let [bindings [['i (lit 0 :int)]]
          body [(expr-stmt (call "foo" [lit 1 :int] :void))]
          stmt (loop-stmt bindings body)]
      (is (re-find #"for\(;;\)" (emit/emit-stmt stmt)))
      (is (re-find #"float i = 0;" (emit/emit-stmt stmt))))))

;; ── 测试函数定义 ──────────────────────────────
(deftest fn-def-test
  (testing "函数输出"
    (let [body [(return-stmt (lit 0.0 :float))]
          func (fn-def "main" [] :float body)]
      (is (= "float main()\n{\n  return 0.0;\n}\n"
             (emit/emit-stmt func))))))

;; ── 测试程序级输出 ────────────────────────────
(deftest program-test
  (testing "完整程序"
    (let [uniforms [(var-decl 'time :float nil)]
          funcs [(fn-def "vs_main" [] :float4
                         [(return-stmt (constructor :float [(lit 1.0 :float) (lit 0.0 :float) (lit 0.0 :float) (lit 1.0 :float)]))])]
          prog (program uniforms funcs)]
      (is (= "float time;\n\nfloat4 vs_main()\n{\n  return float4(1.0, 0.0, 0.0, 1.0);\n}\n"
             (emit/emit-program prog))))))
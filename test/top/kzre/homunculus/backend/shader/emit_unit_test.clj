(ns top.kzre.homunculus.backend.shader.emit-unit-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [top.kzre.homunculus.backend.shader.emit :as emit]
            [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import (clojure.lang ExceptionInfo)))

;; ── 共享后端实例 ──
(def backend (hlsl-backend/->HLSLBackend))

;; ── 辅助函数 ──
(defn- lit [val]
  (m/->LiteralNode val nil nil nil))

(defn- vref [name]
  (m/->VariableNode name nil nil nil))

(defn- typed-var [name ty]
  (m/->VariableNode name {:type ty} nil nil))

(defn- call [fn-node & args]
  (m/->CallNode fn-node (vec args) nil nil nil))

(defn- if-node [test then else]
  (m/->IfNode test then else nil nil nil))

(defn- while-node [test body]
  (m/->WhileNode test body nil nil nil))

(defn- block [& exprs]
  (m/->BlockNode (vec exprs) nil nil nil))

(defn- let-node [bindings body]
  (m/->LetNode (vec bindings) body nil nil nil))

(defn- assign [var val]
  (m/->AssignNode var val nil nil nil))

(defn- define [name lambda-val]
  (m/->DefineNode name lambda-val nil nil nil nil))

(defn- lam [params body]
  (m/->LambdaNode params body [] nil nil nil nil))

(defn- vector-node [items]
  (m/->VectorNode items nil nil nil))

;; ── 字面量测试 ──
(deftest test-literal
  (testing "integer literal"
    (is (= "42" (emit/emit (lit 42) backend))))
  (testing "float literal"
    (is (= "3.14" (emit/emit (lit 3.14) backend))))
  (testing "boolean true"
    (is (= "true" (emit/emit (lit true) backend))))
  (testing "boolean false"
    (is (= "false" (emit/emit (lit false) backend)))))

;; ── 变量测试 ──
(deftest test-variable
  (testing "simple variable"
    (is (= "x" (emit/emit (vref "x") backend))))
  (testing "variable with special chars"
    (is (= "my_var" (emit/emit (vref "my-var") backend)))))

;; ── 调用测试 ──
(deftest test-call
  (testing "simple call"
    (let [node (call (vref "foo") (lit 1) (lit 2))]
      (is (= "foo(1, 2)" (emit/emit node backend)))))
  (testing "call with nested call"
    (let [inner (call (vref "bar") (lit 3))
          node  (call (vref "foo") inner (lit 2))]
      (is (= "foo(bar(3), 2)" (emit/emit node backend))))))

;; ── if 测试 ──
(deftest test-if
  (testing "if with else"
    (let [node (if-node (vref "cond") (lit 1) (lit 2))]
      (is (= "if (cond) { 1 } else { 2 }" (emit/emit node backend)))))
  (testing "if without else"
    (let [node (if-node (vref "cond") (lit 1) nil)]
      (is (= "if (cond) { 1 }" (emit/emit node backend))))))

;; ── while 测试 ──
(deftest test-while
  (let [node (while-node (vref "running") (assign (vref "x") (lit 0)))]
    (is (= "while (running) { x = 0; }" (emit/emit node backend)))))

;; ── block 测试 ──
(deftest test-block
  (let [node (block (lit 1) (lit 2) (lit 3))]
    (is (= "1;\n2;\n3" (emit/emit node backend)))))

;; ── let 测试 ──
(deftest test-let
  (let [x (typed-var "x" (t/->TCon :float))
        val (lit 5.0)
        body (vref "x")
        node (let-node [[x val]] body)]
    (is (str/includes? (emit/emit node backend) "const float x = 5.0;"))
    (is (str/includes? (emit/emit node backend) "x"))))

;; ── 赋值测试 ──
(deftest test-assign
  (let [node (assign (vref "x") (lit 10))]
    (is (= "x = 10;" (emit/emit node backend)))))

;; ── define 测试 ──
(deftest test-define
  (let [param (typed-var "a" (t/->TCon :float))
        body (call (vref "float2") (lit 1) (lit 2))
        body-typed (assoc-in body [:attrs :type] (t/->TCon :float2))
        lam (lam [param] body-typed)
        node (define 'myfn lam)]
    (let [result (emit/emit node backend)]
      (is (str/includes? result "float2 myfn(float a)"))
      (is (str/includes? result "return float2(1, 2);")))))

;; ── 向量测试 ──
(deftest test-vector
  (let [items [(lit 1.0) (lit 2.0) (lit 3.0) (lit 4.0)]
        node (vector-node items)]
    (is (= "float4(1.0, 2.0, 3.0, 4.0)" (emit/emit node backend)))))

;; ── convert 测试 ──
(deftest test-convert
  (let [node {:kind :convert
              :expr (lit 42)
              :attrs {:src-type (t/->TCon :int) :dst-type (t/->TCon :float)}}]
    (is (= "(float)42" (emit/emit node backend)))))

;; ── 异常情况测试 ──
(deftest test-unsupported
  (testing "lambda node should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit (lam [] (lit 1)) backend))))
  (testing "loop node should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit (m/->LoopNode [] (lit 1) nil nil nil) backend))))
  (testing "recur node should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit (m/->RecurNode [(lit 1)] nil nil nil) backend))))
  (testing "try node should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit (m/->TryNode [(lit 1)] [] nil nil nil nil) backend))))
  (testing "catch node should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit (m/->CatchNode (vref "e") (vref "x") [(lit 1)] nil nil nil) backend))))
  (testing "throw node should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit (m/->ThrowNode (lit "err") nil nil nil) backend))))
  (testing "map node should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit (m/->MapNode [(lit :a) (lit 1)] nil nil nil) backend))))
  (testing "default (unknown node kind) should throw"
    (is (thrown? ExceptionInfo
                 (emit/emit {:kind :bogus} backend)))))
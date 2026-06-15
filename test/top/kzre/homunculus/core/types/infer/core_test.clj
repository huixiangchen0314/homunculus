(ns top.kzre.homunculus.core.types.infer.core-test
  "infer-pass 单元测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.infer.methods]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))



;; ── 字面量测试 ──────────────────────────
(deftest literal-test
  (let [frontend (->MockFrontend)]
    (testing "integer"
      (let [root (m/->LiteralNode 42 nil nil nil)
            results (infer/infer [root] :frontend frontend)
            node (first results)]
        (is (tcon? (get-type node) :int64))))
    (testing "float"
      (let [root (m/->LiteralNode 3.14 nil nil nil)
            results (infer/infer [root] :frontend frontend)
            node (first results)]
        (is (tcon? (get-type node) :float64))))
    (testing "string"
      (let [root (m/->LiteralNode "hi" nil nil nil)
            results (infer/infer [root] :frontend frontend)
            node (first results)]
        (is (tcon? (get-type node) :string))))
    (testing "boolean"
      (let [root (m/->LiteralNode true nil nil nil)
            results (infer/infer [root] :frontend frontend)
            node (first results)]
        (is (tcon? (get-type node) :bool))))))

;; ── 变量测试 ──────────────────────────
(deftest variable-test
  (let [frontend (->MockFrontend)]
    (testing "bound variable"
      (let [node (m/->VariableNode "x" nil nil nil)
            [ty result] (infer/local-infer node {:frontend frontend :env {"x" (t/->TCon :int32)}})]
        (is (tcon? ty :int32))
        (is (tcon? (get-type result) :int32))))
    (testing "unbound variable"
      (let [node (m/->VariableNode "y" nil nil nil)
            [ty result] (infer/local-infer node {:frontend frontend :env {}})]
        (is (nil? ty))
        (is (nil? (get-type result)))))))

;; ── 函数调用测试 ──────────────────────────
(deftest call-test
  (let [frontend (->MockFrontend)]
    (testing "successful call"
      (let [fn-ty (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
            fn-node (m/->VariableNode "f" nil nil nil)
            arg-node (m/->LiteralNode 1 nil nil nil)
            call-node (m/->CallNode fn-node [arg-node] nil nil nil)
            [ty result] (infer/local-infer call-node {:frontend frontend :env {"f" fn-ty}})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))
    (testing "unknown function"
      (let [fn-node (m/->VariableNode "g" nil nil nil)
            arg-node (m/->LiteralNode 1 nil nil nil)
            call-node (m/->CallNode fn-node [arg-node] nil nil nil)
            [ty _] (infer/local-infer call-node {:frontend frontend :env {}})]
        (is (nil? ty))))))

;; ── if 测试 ──────────────────────────
(deftest if-test
  (let [frontend (->MockFrontend)]
    (testing "both branches match"
      (let [test-node (m/->LiteralNode true nil nil nil)
            then-node (m/->LiteralNode 1 nil nil nil)
            else-node (m/->LiteralNode 0 nil nil nil)
            if-node (m/->IfNode test-node then-node else-node nil nil nil)
            [ty result] (infer/local-infer if-node {:frontend frontend :env {}})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))
    (testing "test not bool"
      (let [test-node (m/->LiteralNode 42 nil nil nil)
            then-node (m/->LiteralNode 1 nil nil nil)
            if-node (m/->IfNode test-node then-node nil nil nil nil)
            [ty _] (infer/local-infer if-node {:frontend frontend :env {}})]
        (is (nil? ty))))))

;; ── block 测试 ──────────────────────────
(deftest block-test
  (let [frontend (->MockFrontend)
        exprs [(m/->LiteralNode 1 nil nil nil)
               (m/->LiteralNode "hello" nil nil nil)]
        block-node (m/->BlockNode exprs nil nil nil)
        [ty result] (infer/local-infer block-node {:frontend frontend :env {}})]
    (is (tcon? ty :string))
    (is (tcon? (get-type result) :string))))

;; ── let 测试 ──────────────────────────
(deftest let-test
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 10 nil nil nil)
        var-node (m/->VariableNode "x" nil nil nil)
        body-node (m/->VariableNode "x" nil nil nil)
        let-node (m/->LetNode [[var-node val-node]] body-node nil nil nil)
        [ty result] (infer/local-infer let-node {:frontend frontend :env {}})]
    (is (tcon? ty :int64))
    (is (tcon? (get-type result) :int64))))

;; ── define 测试 ──────────────────────────
(deftest define-test
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 100 nil nil nil)
        define-node (m/->DefineNode 'y val-node nil nil nil nil)
        [ty result] (infer/local-infer define-node {:frontend frontend :env {}})]
    (is (tcon? ty :int64))
    (is (tcon? (get-type result) :int64))
    (is (= 'y (:name result)))))

;; ── lambda 测试 ──────────────────────────
(deftest lambda-test
  (let [frontend (->MockFrontend)
        param-node (m/->VariableNode "x" nil nil nil)
        body-node (m/->VariableNode "x" nil nil nil)
        lambda-node (m/->LambdaNode [param-node] body-node [] nil nil nil nil)
        [ty _] (infer/local-infer lambda-node {:frontend frontend :env {}})]
    (is (nil? ty))))


;; ── while 测试 ──────────────────────────
(deftest while-test
  (let [frontend (->MockFrontend)]
    (testing "simple while loop"
      (let [test-node (m/->LiteralNode true nil nil nil)   ;; 条件恒为真
            body-node (m/->LiteralNode 1 nil nil nil)      ;; 体返回整数
            while-node (m/->WhileNode test-node body-node nil nil nil)
            [ty result] (infer/local-infer while-node {:frontend frontend :env {}})]
        (is (tcon? ty :int64))                 ;; body 类型为 int64
        (is (tcon? (get-type result) :int64))))
    (testing "while with unknown body"
      (let [test-node (m/->LiteralNode true nil nil nil)
            body-node (m/->VariableNode "x" nil nil nil)   ;; x 未绑定
            while-node (m/->WhileNode test-node body-node nil nil nil)
            [ty result] (infer/local-infer while-node {:frontend frontend :env {}})]
        (is (nil? ty))
        (is (nil? (get-type result)))))))
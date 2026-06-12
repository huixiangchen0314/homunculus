(ns top.kzre.homunculus.core.types.check.core-test
  "check-pass 单元测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]  ;; 使用公共 MockBackend, MockFrontend, get-type, tcon?, convert?
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.methods]            ;; 注册所有方法
            [top.kzre.homunculus.core.ir2.model :as m])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

;; ── 叶子节点测试 ──────────────────────────
(deftest literal-no-expected-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit (m/->LiteralNode 42 nil nil  nil)
        lit-typed (assoc-in lit [:attrs :type] (t/->TCon :int64))
        result (check/check lit-typed nil context)]
    (is (not (convert? result)))
    (is (= result lit-typed))))

(deftest literal-same-type-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit (m/->LiteralNode 42 nil nil  nil)
        lit-typed (assoc-in lit [:attrs :type] (t/->TCon :int64))
        result (check/check lit-typed (t/->TCon :int64) context)]
    (is (not (convert? result)))
    (is (= result lit-typed))))

(deftest literal-convert-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit (m/->LiteralNode 42 nil nil  nil)
        lit-typed (assoc-in lit [:attrs :type] (t/->TCon :int64))
        result (check/check lit-typed (t/->TCon :float32) context)]
    (is (convert? result))
    (is (= (-> result :expr) lit-typed))
    (is (= (get-in result [:attrs :type]) (t/->TCon :float32)))
    (is (= (convert-cost result) 1))))

(deftest literal-no-convert-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit (m/->LiteralNode "hello" nil nil  nil)
        lit-typed (assoc-in lit [:attrs :type] (t/->TCon :string))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (check/check lit-typed (t/->TCon :float32) context)))))

;; ── 复合节点测试 ──────────────────────────
(deftest if-check-test
  (let [backend (->MockBackend)
        context {:backend backend}
        test-node (m/->LiteralNode true nil nil  nil)
        test-typed (assoc-in test-node [:attrs :type] (t/->TCon :bool))
        then-node (m/->LiteralNode 42 nil nil  nil)
        then-typed (assoc-in then-node [:attrs :type] (t/->TCon :int64))
        else-node (m/->LiteralNode 0 nil nil  nil)
        else-typed (assoc-in else-node [:attrs :type] (t/->TCon :int64))
        if-node (m/->IfNode test-typed then-typed else-typed nil nil  nil)
        result (check/check if-node (t/->TCon :float32) context)]
    (is (not (convert? result)))
    (is (convert? (:then result)))
    (is (convert? (:else result)))))

;; ── 程序入口测试 ──────────────────────────
(deftest check-program-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit1 (m/->LiteralNode 1 nil nil  nil)
        lit1-typed (assoc-in lit1 [:attrs :type] (t/->TCon :int64))
        lit2 (m/->LiteralNode 2 nil nil  nil)
        lit2-typed (assoc-in lit2 [:attrs :type] (t/->TCon :int64))
        roots [lit1-typed lit2-typed]
        checked (check/check-program roots context)]
    (is (= 2 (count checked)))
    (is (not (convert? (first checked))))
    (is (not (convert? (second checked))))))


(deftest while-check-test
  (let [backend (->MockBackend)
        context {:backend backend}
        test-node (m/->LiteralNode true nil nil nil)
        test-typed (assoc-in test-node [:attrs :type] (t/->TCon :bool))
        body-node (m/->LiteralNode 42 nil nil nil)
        body-typed (assoc-in body-node [:attrs :type] (t/->TCon :int64))
        while-node (m/->WhileNode test-typed body-typed nil nil nil)
        result (check/check while-node nil context)]
    ;; 检查后 test 和 body 应保持原样，无转换（因为类型匹配）
    (is (not (convert? result)))
    (is (= (:test result) test-typed))
    (is (= (:body result) body-typed))))

(deftest while-check-convert-test
  (let [backend (->MockBackend)
        context {:backend backend}
        test-node (m/->LiteralNode true nil nil nil)
        test-typed (assoc-in test-node [:attrs :type] (t/->TCon :bool))
        ;; body 期望是 float，但实际是 int
        body-node (m/->LiteralNode 42 nil nil nil)
        body-typed (assoc-in body-node [:attrs :type] (t/->TCon :int64))
        while-node (m/->WhileNode test-typed body-typed nil nil nil)
        ;; 检查时没有期望类型，但 test 会检查为 bool
        result (check/check while-node nil context)]
    (is (not (convert? result)))
    ;; test 没有被转换，body 也没有被转换（因为无期望）
    (is (= (get-in result [:test :attrs :type]) (t/->TCon :bool)))
    (is (= (get-in result [:body :attrs :type]) (t/->TCon :int64)))))
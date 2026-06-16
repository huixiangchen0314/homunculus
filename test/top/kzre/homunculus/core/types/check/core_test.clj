(ns top.kzre.homunculus.core.types.check.core-test
  "check-pass 单元测试。"
  (:require
    [clojure.test :refer :all]
    [top.kzre.homunculus.core.ir2.model :as m]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.check.core :as check]
    [top.kzre.homunculus.core.types.check.methods]          ;; 注册所有方法
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.core.types.test-utils :refer :all] ;; 使用公共 MockBackend, MockFrontend, get-type, tcon?, convert?
    [top.kzre.homunculus.core.types.test-utils :as tu]

    [top.kzre.homunculus.core.types.type :as type]
    [top.kzre.homunculus.core.types.type :as ty]))

;; ── 叶子节点测试 ──────────────────────────
(deftest literal-no-expected-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit (m/->LiteralNode 42 nil nil  nil)
        lit-typed (assoc-in lit [:attrs :type] (t/->TCon :int64))
        result (check/check-node lit-typed nil context)]
    (is (not (convert? result)))
    (is (= result lit-typed))))

(deftest literal-same-type-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit (m/->LiteralNode 42 nil nil  nil)
        lit-typed (assoc-in lit [:attrs :type] (t/->TCon :int64))
        result (check/check-node lit-typed (t/->TCon :int64) context)]
    (is (not (convert? result)))
    (is (= result lit-typed))))


(deftest literal-no-convert-test
  (let [backend (->MockBackend)
        context {:backend backend}
        lit (m/->LiteralNode "hello" nil nil  nil)
        lit-typed (assoc-in lit [:attrs :type] (t/->TCon :string))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (check/check-node lit-typed (t/->TCon :float32) context)))))

;; ── 复合节点测试 ──────────────────────────


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
        result (check/check-node while-node nil context)]
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
        result (check/check-node while-node nil context)]
    (is (not (convert? result)))
    ;; test 没有被转换，body 也没有被转换（因为无期望）
    (is (= (get-in result [:test :attrs :type]) (t/->TCon :bool)))
    (is (= (get-in result [:body :attrs :type]) (t/->TCon :int64)))))


(def backend (tu/->MockBackend))

(deftest check-type-matching
  (testing "exact match returns node unchanged"
    (let [node (-> (n/->literal 42 {} {} nil)
                   (ty/set-type! (t/->TCon :int64)))]
      (let [result (check/check-type node (t/->TCon :int64) {:backend backend})]
        (is (= node result))
        (is (not (n/convert-node? result)))))))


(deftest check-type-with-conversion
  (testing "int64 -> float32 conversion creates convert node"
    (let [node (-> (n/->literal 42 {} {} nil)
                   (ty/set-type! (t/->TCon :int64)))]  ;; 正确使用 -> 绑定返回值
      (let [result (check/check-type node (t/->TCon :float32) {:backend backend})]
        (is (n/convert-node? result))
        (is (= (t/->TCon :int64) (-> result :attrs :src-type)))
        (is (= (t/->TCon :float32) (-> result :attrs :type)))
        (is (= 1 (-> result :attrs :cost)))))))

(deftest check-type-no-conversion-throws
  (let [node (n/->literal true {} {} nil)]
    (ty/set-type! node (t/->TCon :bool))
    (testing "bool -> int64 has no conversion, must throw"
      (is (thrown? Exception
                   (check/check-type node (t/->TCon :int64) {:backend backend}))))))
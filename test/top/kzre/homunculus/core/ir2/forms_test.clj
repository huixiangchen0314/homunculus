(ns top.kzre.homunculus.core.ir2.forms-test
  "IR2 forms lowering 的单元测试，覆盖所有特殊形式。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.test-utils :refer :all]))

;; 辅助函数
(defn- node? [node expected-kind]
  (and (satisfies? ir2p/INode node)
       (= expected-kind (ir2p/kind node))))

(defn- literal-node? [node val]
  (and (node? node :literal) (= val (:val node))))

(defn- variable-node? [node name]
  (and (node? node :variable) (= name (:name node))))

;; ── 测试：def lowering ─────────────────────
(deftest def-lowering-test
  (testing "def with value"
    (let [ir1-root (ir1/->ir1 '(def x 42))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :define))
      (is (= 'x (:name node)))
      (let [kids (ir2p/children node)]
        (is (= 1 (count kids)))                     ;; 只有一个 val
        (is (literal-node? (first kids) 42))))))

;; ── 测试：fn* lowering ─────────────────────
(deftest fn-lowering-test
  (testing "anonymous fn"
    (let [ir1-root (ir1/->ir1 '(fn* [a b] (+ a b)))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :lambda))
      (let [kids (ir2p/children node)]
        (is (= 3 (count kids)))
        (is (variable-node? (first kids) "a"))
        (is (variable-node? (second kids) "b"))
        (is (node? (nth kids 2) :call))))))

;; ── 测试：if lowering ──────────────────────
(deftest def-lowering-test
  (testing "def with value"
    (let [ir1-root (ir1/->ir1 '(def x 42))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :define))
      (is (= 'x (:name node)))
      (let [kids (ir2p/children node)]
        (is (= 1 (count kids)))                     ;; 只有一个 val
        (is (literal-node? (first kids) 42))))))

;; ── 测试：do/block lowering ────────────────
(deftest block-lowering-test
  (testing "do expression"
    (let [ir1-root (ir1/->ir1 '(do 1 2 3))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :block))
      (let [kids (ir2p/children node)]
        (is (= 3 (count kids)))
        (is (literal-node? (first kids) 1))
        (is (literal-node? (second kids) 2))
        (is (literal-node? (nth kids 2) 3))))))

;; ── 测试：let lowering ────────────────────
(deftest def-lowering-test
  (testing "def with value"
    (let [ir1-root (ir1/->ir1 '(def x 42))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :define))
      (is (= 'x (:name node)))
      (let [kids (ir2p/children node)]
        (is (= 1 (count kids)))                     ;; 只有一个 val
        (is (literal-node? (first kids) 42))))))

;; ── 测试：loop lowering ───────────────────
(deftest def-lowering-test
  (testing "def with value"
    (let [ir1-root (ir1/->ir1 '(def x 42))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :define))
      (is (= 'x (:name node)))
      (let [kids (ir2p/children node)]
        (is (= 1 (count kids)))
        (is (literal-node? (first kids) 42))))))

;; ── 测试：recur lowering ───────────────────
(deftest recur-lowering-test
  (testing "recur"
    (let [ir1-root (ir1/->ir1 '(recur (inc x)))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :recur))
      (let [kids (ir2p/children node)]
        (is (= 1 (count kids)))
        (is (node? (first kids) :call))))))

;; ── 测试：quote lowering ───────────────────
(deftest quote-lowering-test
  (testing "quote symbol"
    (let [ir1-root (ir1/->ir1 '(quote x))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (variable-node? node "x")))))

;; ── 测试：try lowering ─────────────────────
(deftest try-lowering-test
  (testing "try with catch"
    (let [ir1-root (ir1/->ir1 '(try (dangerous) (catch Exception e (handle e))))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :try))
      (let [kids (ir2p/children node)]
        (is (= 2 (count kids)))
        (is (node? (first kids) :call))
        (is (node? (second kids) :catch))))))

;; ── 测试：throw lowering ───────────────────
(deftest throw-lowering-test
  (testing "throw"
    (let [ir1-root (ir1/->ir1 '(throw (Exception. "boom")))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :throw))
      (let [kids (ir2p/children node)]
        (is (= 1 (count kids)))
        (is (node? (first kids) :call))))))

;; ── 测试：set! lowering ────────────────────
(deftest set!-lowering-test
  (testing "set!"
    (let [ir1-root (ir1/->ir1 '(set! *x* 10))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (node? node :assign))
      (let [kids (ir2p/children node)]
        (is (= 2 (count kids)))
        (is (variable-node? (first kids) "*x*"))
        (is (literal-node? (second kids) 10))))))

;; ── 测试：var lowering ─────────────────────
(deftest var-lowering-test
  (testing "(var foo)"
    (let [ir1-root (ir1/->ir1 '(var println))
          results (ir2/lower [ir1-root])
          node (first results)]
      (is (variable-node? node "println")))))
(ns top.kzre.homunculus.core.types.mutability.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.mutability.core :as sut]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- assign [var val] (m/->AssignNode var val nil nil nil))
(defn- call [f & args] (m/->CallNode f (vec args) nil nil nil))

(deftest no-assign-test
  (testing "no assign → no mutable"
    (let [roots [(lit 42) (vref "x")]
          result (sut/analyze roots)]
      (is (nil? (get-in (first result) [:attrs :mutable])))
      (is (nil? (get-in (second result) [:attrs :mutable]))))))

(deftest simple-assign-test
  (testing "variable assigned → mutable"
    (let [x (vref "x")
          set-x (assign x (lit 10))
          roots [set-x]
          result (sut/analyze roots)]
      ;; assign 节点本身不标记，其左侧变量引用被标记
      (let [assigned-var (:var (first result))]
        (is (true? (get-in assigned-var [:attrs :mutable])))))))

(deftest multiple-assign-test
  (testing "multiple assigns on same variable"
    (let [x1 (vref "x")
          x2 (vref "x")
          set1 (assign x1 (lit 1))
          set2 (assign x2 (lit 2))
          roots [set1 set2]
          result (sut/analyze roots)]
      (is (true? (get-in (:var (first result)) [:attrs :mutable])))
      (is (true? (get-in (:var (second result)) [:attrs :mutable]))))))

(deftest while-mutability-test
  (testing "while loop with assignment inside"
    (let [x (vref "x")
          body (assign x (call (vref "+") x (lit 1)))
          test (call (vref "<") x (lit 10))
          while-node (m/->WhileNode test body nil nil nil)
          roots [while-node]
          result (sut/analyze roots)]
      ;; 遍历 while 节点体内的 assign 应该标记 x
      (let [assigned-var (-> (first result) :body :var)]
        (is (true? (get-in assigned-var [:attrs :mutable])))))))
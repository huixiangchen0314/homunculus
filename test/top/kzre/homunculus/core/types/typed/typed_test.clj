(ns top.kzre.homunculus.core.types.typed.typed-test
  (:require
   [clojure.test :refer :all]
   [top.kzre.homunculus.core.ir2.model :as m]
   [top.kzre.homunculus.core.types.model :as t]
   [top.kzre.homunculus.core.types.test-utils :refer [tcon?]]
   [top.kzre.homunculus.core.types.typed.core :as typed]
   [top.kzre.homunculus.core.types.typed.methods] ;; 加载多方法
   [top.kzre.homunculus.core.types.typed.unify :as u]))

;; 辅助函数
(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- while-node [test body] (m/->WhileNode test body nil nil nil))

(deftest while-typed-test
  (testing "HM inference for while"
    (let [test-node (lit true)
          body-node (lit 42)
          node (while-node test-node body-node)
          context {:env {}}
          [ty inferred-node s] (typed/infer node context)]
      ;; while 的整体类型为 body 类型（替换后）
      (is (some? ty))
      ;; test 类型在替换后应被统一为 bool
      (is (tcon? (u/substitute (get-in (:test inferred-node) [:attrs :type]) s) :bool))
      ;; body 类型在替换后应有值（具体类型未知，但不为 nil）
      (is (some? (u/substitute (get-in (:body inferred-node) [:attrs :type]) s))))))

(deftest assign-typed-test
  (testing "HM inference for assign"
    (let [var-node (vref "x")
          val-node (lit 10)
          assign-node (m/->AssignNode var-node val-node nil nil nil)
          context {:env {"x" (t/->TCon :int64)}}
          [ty inferred-node s] (typed/infer assign-node context)]
      ;; assign 的类型应为 nil
      (is (tcon? ty :nil))
      ;; 替换中 var 和 val 类型应统一（这里 var 类型是 int64，val 类型在替换后也应为 int64）
      (let [var-ty' (u/substitute (get-in (:var inferred-node) [:attrs :type]) s)
            val-ty' (u/substitute (get-in (:val inferred-node) [:attrs :type]) s)]
        (is (tcon? var-ty' :int64))
        (is (= var-ty' val-ty'))))))
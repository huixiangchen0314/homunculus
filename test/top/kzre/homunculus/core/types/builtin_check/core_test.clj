(ns top.kzre.homunculus.core.types.builtin-check.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.builtin-check.core :as sut]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- call [f & args] (m/->CallNode f (vec args) nil nil nil))

(def builtins
  {'+ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   'inc (t/->TFun (t/->TCon :int64) (t/->TCon :int64))})

(deftest allowed-function-test
  (testing "known function passes"
    (let [roots [(call (vref "+") (lit 1) (lit 2))]
          result (sut/check roots builtins)]
      (is (= 1 (count result)))
      (let [node (first result)]
        (is (= (:name (:fn node)) "+"))
        (is (get-in node [:attrs :builtin-fn]))))))

(deftest unknown-function-test
  (testing "unknown function throws"
    (let [roots [(call (vref "unknown") (lit 1))]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (sut/check roots builtins))))))

(deftest lambda-call-ignored-test
  (testing "lambda call is ignored (no builtin check)"
    (let [lam (m/->LambdaNode [(vref "x")] (vref "x") [] nil nil nil nil)
          call-node (call lam (lit 5))
          roots [call-node]
          result (sut/check roots builtins)]
      (is (= 1 (count result)))
      (let [node (first result)]
        (is (nil? (get-in node [:attrs :builtin-fn])))))))
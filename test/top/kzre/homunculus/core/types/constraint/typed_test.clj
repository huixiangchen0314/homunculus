(ns top.kzre.homunculus.core.types.constraint.typed-test
  (:require
    [clojure.test :refer :all]
    [top.kzre.homunculus.core.ir2.model :as m]
    [top.kzre.homunculus.core.types.constraint.solve :as cs]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.core.types.test-utils :refer [get-type tcon?]]))

(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- while-node [test body] (m/->WhileNode test body nil nil nil))
(defn- process-one [node context] (first (cs/process [node] context)))

(deftest while-typed-test
  (let [test-node (lit true)
        body-node (lit 42)
        node (while-node test-node body-node)
        result (process-one node {:frontend nil :env {}})]
    (is (some? (get-type result)))
    (is (tcon? (get-type (:test result)) :bool))
    (is (some? (get-type (:body result))))))

(deftest assign-typed-test
  (let [var-node (vref "x")
        val-node (lit 10)
        assign-node (m/->AssignNode var-node val-node nil nil nil)
        result (process-one assign-node {:frontend nil :env {"x" (t/->TCon :int64)}})]
    (is (tcon? (get-type result) :nil))
    (let [var-ty (get-type (:var result))
          val-ty (get-type (:val result))]
      (is (tcon? var-ty :int64))
      (is (= var-ty val-ty)))))
(ns top.kzre.homunculus.core.types.constraint.loop-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api]   ;; 加载所有 defmethod
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))

(defn- run-solve
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest loop-constraint-generation
  (testing "Loop with constant body returns body type"
    (let [var (n/->variable 'i {} {} nil)
          val (n/->literal 0 {} {} nil)   ;; int64
          body (n/->literal 42 {} {} nil) ;; int64
          loop-node (n/->loop [[var val]] body {} {} nil)
          typed-loop (run-solve loop-node {})
          loop-ty (ty/get-type typed-loop)]
      (is (= :int64 (:name loop-ty)))))

  (testing "Loop body can access loop variable, type matches initial value"
    (let [var (n/->variable 'acc {} {} nil)
          val (n/->literal 0 {} {} nil)   ;; int64
          body (n/->variable 'acc {} {} nil)  ;; reference to acc
          loop-node (n/->loop [[var val]] body {} {} nil)
          typed-loop (run-solve loop-node {})
          loop-ty (ty/get-type typed-loop)]
      (is (= :int64 (:name loop-ty))))

    (testing "Loop variable type is a fresh TVar that gets unified with initial value type"
      ;; 我们无法直接检查环境内部的绑定类型，但通过 body 引用，可以确信类型正确传播
      (let [var (n/->variable 'x {} {} nil)
            val (n/->literal 10 {} {} nil) ;; int64
            body (n/->variable 'x {} {} nil) ;; body 引用 x
            loop-node (n/->loop [[var val]] body {} {} nil)
            typed-loop (run-solve loop-node {})
            typed-body (n/loop-body typed-loop)
            body-ty (ty/get-type typed-body)]
        (is (= :int64 (:name body-ty)))))

    (testing "Multiple loop bindings — body refers to first binding"
      (let [var1 (n/->variable 'a {} {} nil)
            val1 (n/->literal 1 {} {} nil)   ;; int64
            var2 (n/->variable 'b {} {} nil)
            val2 (n/->literal true {} {} nil) ;; bool
            body (n/->variable 'a {} {} nil)  ;; reference to a
            loop-node (n/->loop [[var1 val1] [var2 val2]] body {} {} nil)
            typed-loop (run-solve loop-node {})
            loop-ty (ty/get-type typed-loop)]
        (is (= :int64 (:name loop-ty)))))))
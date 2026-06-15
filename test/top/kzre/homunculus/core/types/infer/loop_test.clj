(ns top.kzre.homunculus.core.types.infer.loop-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.loop]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.infer.methods.variable]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest loop-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "loop returns body type"
      (let [var (n/->variable 'i {} {} nil)
            val (n/->literal 0 {} {} nil)
            body (n/->literal 42 {} {} nil)
            loop-node (n/->loop [[var val]] body {} {} nil)
            [ty new-node] (infer/local-infer loop-node context)]
        (is (= :int64 (:name ty)))))
    (testing "loop body can access loop binding"
      (let [var (n/->variable 'acc {} {} nil)
            val (n/->literal 0 {} {} nil)
            body (n/->variable 'acc {} {} nil)
            loop-node (n/->loop [[var val]] body {} {} nil)
            [ty new-node] (infer/local-infer loop-node context)]
        (is (= :int64 (:name ty)))))))
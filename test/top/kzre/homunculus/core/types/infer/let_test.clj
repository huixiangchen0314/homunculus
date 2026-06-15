(ns top.kzre.homunculus.core.types.infer.let-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.let]
            [top.kzre.homunculus.core.types.infer.methods.variable]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest let-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "let body type is returned"
      (let [var (n/->variable 'x {} {} nil)
            val (n/->literal 42 {} {} nil)
            body (n/->variable 'x {} {} nil)
            let-node (n/->let [[var val]] body {} {} nil)
            [ty new-node] (infer/local-infer let-node context)]
        (is (= :int64 (:name ty)))))
    (testing "let with multiple bindings"
      (let [var1 (n/->variable 'a {} {} nil)
            val1 (n/->literal 1 {} {} nil)
            var2 (n/->variable 'b {} {} nil)
            val2 (n/->literal true {} {} nil)
            body (n/->variable 'a {} {} nil)
            let-node (n/->let [[var1 val1] [var2 val2]] body {} {} nil)
            [ty new-node] (infer/local-infer let-node context)]
        (is (= :int64 (:name ty)))))))
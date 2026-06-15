(ns top.kzre.homunculus.core.types.infer.while-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.while]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest while-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "while returns body type"
      (let [test (n/->literal true {} {} nil)
            body (n/->literal 42 {} {} nil)
            while-node (n/->while test body {} {} nil)
            [ty new-node] (infer/local-infer while-node context)]
        (is (= :int64 (:name ty)))))
    (testing "while with non-bool test still infers body type"
      (let [test (n/->literal 1 {} {} nil)
            body (n/->literal 42 {} {} nil)
            while-node (n/->while test body {} {} nil)
            [ty new-node] (infer/local-infer while-node context)]
        (is (= :int64 (:name ty)))))))
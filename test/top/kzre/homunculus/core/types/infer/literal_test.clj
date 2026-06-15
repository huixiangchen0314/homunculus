(ns top.kzre.homunculus.core.types.infer.literal-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest literal-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "integer literal gets :int64"
      (let [node (n/->literal 42 {} {} nil)
            [ty new-node] (infer/local-infer node context)]
        (is (some? ty))
        (is (= :int64 (:name ty)))))
    (testing "string literal gets :string"
      (let [node (n/->literal "hello" {} {} nil)
            [ty new-node] (infer/local-infer node context)]
        (is (= :string (:name ty)))))
    (testing "boolean literal gets :bool"
      (let [node (n/->literal false {} {} nil)
            [ty new-node] (infer/local-infer node context)]
        (is (= :bool (:name ty)))))))
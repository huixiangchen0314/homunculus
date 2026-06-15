(ns top.kzre.homunculus.core.types.infer.define-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.define]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest define-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "define node type is value type"
      (let [val (n/->literal 42 {} {} nil)
            define (n/->define 'answer val nil {} {} nil)
            [ty new-node] (infer/local-infer define context)]
        (is (= :int64 (:name ty)))
        (is (= 'answer (:name new-node)))))))
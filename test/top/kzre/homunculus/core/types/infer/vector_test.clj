(ns top.kzre.homunculus.core.types.infer.vector-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.vector]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.test-utils :as tu])
  (:import (top.kzre.homunculus.core.types.model THeteroVec)))

(deftest vector-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "non-empty vector infers THeteroVec with element types"
      (let [items [(n/->literal 1 {} {} nil) (n/->literal "a" {} {} nil)]
            vec-node (n/->vector items {} {} nil)
            [ty new-node] (infer/local-infer vec-node context)]
        (is (instance? THeteroVec ty))
        (is (= [:int64 :string] (mapv :name (:types ty))))))
    (testing "empty vector yields THeteroVec with empty types"
      (let [vec-node (n/->vector [] {} {} nil)
            [ty new-node] (infer/local-infer vec-node context)]
        (is (instance? THeteroVec ty))
        (is (empty? (:types ty)))))))
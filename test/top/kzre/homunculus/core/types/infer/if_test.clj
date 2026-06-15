(ns top.kzre.homunculus.core.types.infer.if-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.if]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest if-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "if with bool test and matching branches gets branch type"
      (let [test (n/->literal true {} {} nil)
            then (n/->literal 1 {} {} nil)
            else (n/->literal 2 {} {} nil)
            node (n/->if test then else {} {} nil)
            [ty new-node] (infer/local-infer node context)]
        (is (= :int64 (:name ty)))))
    (testing "if without else, with then typed, gets then type"
      (let [test (n/->literal true {} {} nil)
            then (n/->literal 42 {} {} nil)
            node (n/->if test then nil {} {} nil)
            [ty new-node] (infer/local-infer node context)]
        (is (= :int64 (:name ty)))))
    (testing "if with non-bool test returns nil"
      (let [test (n/->literal 1 {} {} nil)   ;; int, not bool
            then (n/->literal 2 {} {} nil)
            node (n/->if test then nil {} {} nil)
            [ty new-node] (infer/local-infer node context)]
        (is (nil? ty))))))
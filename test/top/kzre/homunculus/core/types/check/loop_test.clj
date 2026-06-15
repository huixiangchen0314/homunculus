(ns top.kzre.homunculus.core.types.check.loop-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.check.api]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))
(def backend  (tu/->MockBackend))

(deftest loop-recur-check
  (let [ctx {:frontend frontend :backend backend}]
    (testing "loop passes binding types to recur"
      (let [loop-var (-> (n/->variable 'i {} {} nil) (ty/set-type! (t/->TCon :int64)))
            init-val (-> (n/->literal 0 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            recur-arg (-> (n/->literal 1 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            recur-node (n/->recur [recur-arg] {} {} nil)
            loop-node (n/->loop [[loop-var init-val]] recur-node {} {} nil)]
        (is (some? (check/check loop-node nil ctx)))))

    (testing "recur argument type mismatch with loop var should throw"
      (let [loop-var (-> (n/->variable 'i {} {} nil) (ty/set-type! (t/->TCon :int64)))
            init-val (-> (n/->literal 0 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            recur-arg (-> (n/->literal true {} {} nil) (ty/set-type! (t/->TCon :bool)))
            recur-node (n/->recur [recur-arg] {} {} nil)
            loop-node (n/->loop [[loop-var init-val]] recur-node {} {} nil)]
        (is (thrown? Exception
                     (check/check loop-node nil ctx)))))))
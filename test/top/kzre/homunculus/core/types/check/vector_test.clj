(ns top.kzre.homunculus.core.types.check.vector-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.api]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))
(def backend  (tu/->MockBackend))

(deftest vector-check
  (let [ctx {:frontend frontend :backend backend}]
    (testing "hetero vector with matching expected hetero-vec passes"
      (let [item1 (-> (n/->literal 1 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            item2 (-> (n/->literal "a" {} {} nil) (ty/set-type! (t/->TCon :string)))
            vec (-> (n/->vector [item1 item2] {} {} nil)
                    (ty/set-type! (t/->THeteroVec [(t/->TCon :int64) (t/->TCon :string)])))
            expected (t/->THeteroVec [(t/->TCon :int64) (t/->TCon :string)])
            res (check/check-node vec expected ctx)]
        (is (not (n/convert-node? res)))
        (is (= expected (ty/get-type res)))))

    (testing "hetero vector with element requiring conversion inserts convert"
      (let [item1 (-> (n/->literal 1 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            item2 (-> (n/->literal "a" {} {} nil) (ty/set-type! (t/->TCon :string)))
            vec (-> (n/->vector [item1 item2] {} {} nil)
                    (ty/set-type! (t/->THeteroVec [(t/->TCon :int64) (t/->TCon :string)])))
            expected (t/->THeteroVec [(t/->TCon :float32) (t/->TCon :string)])]
        (let [res (check/check-node vec expected ctx)
              items (:items res)
              first-item (first items)]
          (is (n/convert-node? first-item))
          (is (= :float32 (-> first-item ty/get-type :name))))))

    (testing "empty vector matches empty hetero-vec"
      (let [vec (-> (n/->vector [] {} {} nil) (ty/set-type! (t/->THeteroVec [])))
            expected (t/->THeteroVec [])
            res (check/check-node vec expected ctx)]
        (is (= [] (:items res)))))

    (testing "unified element type vector (TContainer :vector) forces all elements to convert"
      (let [item1 (-> (n/->literal 1 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            item2 (-> (n/->literal 2 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            vec (-> (n/->vector [item1 item2] {} {} nil)
                    (ty/set-type! (t/->THeteroVec [(t/->TCon :int64) (t/->TCon :int64)])))
            expected (t/->TContainer :vector (t/->TCon :float32) (t/->VariableLength))
            res (check/check-node vec expected ctx)
            items (:items res)]
        (is (every? n/convert-node? items))
        (is (every? #(= :float32 (-> % ty/get-type :name)) items))))))
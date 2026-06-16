(ns top.kzre.homunculus.core.types.check.literal-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.api]          ;; 加载所有 defmethod
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))
(def backend  (tu/->MockBackend))

(deftest literal-check
  (let [ctx {:frontend frontend :backend backend}]
    (testing "no expected type, node unchanged"
      (let [node (-> (n/->literal 42 {} {} nil)
                     (ty/set-type! (t/->TCon :int64)))
            res (check/check-node node nil ctx)]
        (is (= :int64 (-> res ty/get-type :name)))))

    (testing "expected type matches, node unchanged"
      (let [node (-> (n/->literal 42 {} {} nil)
                     (ty/set-type! (t/->TCon :int64)))
            res (check/check-node node (t/->TCon :int64) ctx)]
        (is (not (n/convert-node? res)))))

    (testing "expected type requires conversion, convert node inserted"
      (let [node (-> (n/->literal 42 {} {} nil)
                     (ty/set-type! (t/->TCon :int64)))
            res (check/check-node node (t/->TCon :float32) ctx)]
        (is (n/convert-node? res))
        (is (= :float32 (-> res ty/get-type :name)))))))
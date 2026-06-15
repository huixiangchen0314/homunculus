(ns top.kzre.homunculus.core.types.check.while-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.api]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))
(def backend  (tu/->MockBackend))

(deftest while-check
  (let [ctx {:frontend frontend :backend backend}]
    (testing "while with nil expected passes"
      (let [test (-> (n/->literal true {} {} nil) (ty/set-type! (t/->TCon :bool)))
            body (-> (n/->literal 42 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            while-node (-> (n/->while test body {} {} nil) (ty/set-type! (t/->TCon :nil)))
            res (check/check while-node (t/->TCon :nil) ctx)]
        (is (not (n/convert-node? res)))))

    (testing "while with non-nil expected throws"
      (let [test (-> (n/->literal true {} {} nil) (ty/set-type! (t/->TCon :bool)))
            body (-> (n/->literal 42 {} {} nil) (ty/set-type! (t/->TCon :int64)))
            while-node (-> (n/->while test body {} {} nil) (ty/set-type! (t/->TCon :nil)))]
        (is (thrown? Exception
                     (check/check while-node (t/->TCon :int64) ctx)))))))
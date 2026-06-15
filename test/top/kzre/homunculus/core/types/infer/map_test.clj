(ns top.kzre.homunculus.core.types.infer.map-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.map]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :as tu])
  (:import (top.kzre.homunculus.core.types.model THeteroMap)))

(deftest map-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "map with one entry infers THeteroMap"
      (let [kvs [(n/->literal :a {} {} nil) (n/->literal 1 {} {} nil)]
            map-node (n/->map kvs {} {} nil)
            [ty new-node] (infer/local-infer map-node context)]
        (is (instance? THeteroMap ty))
        (is (= [[:keyword :int64]] (mapv (fn [[k v]] [(-> k :name) (-> v :name)]) (:entries ty))))))))
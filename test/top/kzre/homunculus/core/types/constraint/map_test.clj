(ns top.kzre.homunculus.core.types.constraint.map-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api] ;; 加载所有 defmethod
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty])
  (:import (top.kzre.homunculus.core.types.model THeteroMap)))

(def frontend (tu/->MockFrontend))

(defn- run-solve
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest map-constraint-generation
  (testing "Non-empty heterogeneous map type"
    (let [kvs [(n/->literal :a {} {} nil) (n/->literal 1 {} {} nil)
               (n/->literal :b {} {} nil) (n/->literal "hello" {} {} nil)]
          map-node (n/->map kvs {} {} nil)
          typed-map (run-solve map-node {})
          map-ty (ty/get-type typed-map)]
      (is (instance? THeteroMap map-ty))
      (is (= [[:keyword :int64] [:keyword :string]]
             (mapv (fn [[k v]] [(ty/type-sym k) (ty/type-sym v)]) (:entries map-ty))))))

  (testing "Empty map type"
    (let [map-node (n/->map [] {} {} nil)
          typed-map (run-solve map-node {})
          map-ty (ty/get-type typed-map)]
      (is (instance? THeteroMap map-ty))
      (is (empty? (:entries map-ty)))))

  (testing "Map with unresolved variable in value retains TVar"
    (let [k-node (n/->literal :key {} {} nil)
          v-node (n/->variable 'x {} {} nil)   ;; 未绑定
          map-node (n/->map [k-node v-node] {} {} nil)
          typed-map (run-solve map-node {})
          map-ty (ty/get-type typed-map)]
      (is (instance? THeteroMap map-ty))
      (let [[[k-ty v-ty]] (:entries map-ty)]
        (is (= :keyword (ty/type-sym k-ty)))
        (is (ty/var-type? v-ty))))))
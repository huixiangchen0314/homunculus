(ns top.kzre.homunculus.core.types.constraint.vector-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api] ;; 注册所有方法
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty])
  (:import (top.kzre.homunculus.core.types.model THeteroVec)))

(def frontend (tu/->MockFrontend))

(defn- run-solve
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest vector-constraint-generation
  (testing "Non-empty heterogeneous vector type"
    (let [items [(n/->literal 1 {} {} nil) (n/->literal "a" {} {} nil)]
          vec-node (n/->vector items {} {} nil)
          typed-vec (run-solve vec-node {})
          vec-ty (ty/get-type typed-vec)]
      (is (instance? THeteroVec vec-ty))
      (is (= [:int64 :string] (mapv :name (:types vec-ty))))))

  (testing "Empty vector type"
    (let [vec-node (n/->vector [] {} {} nil)
          typed-vec (run-solve vec-node {})
          vec-ty (ty/get-type typed-vec)]
      (is (instance? THeteroVec vec-ty))
      (is (empty? (:types vec-ty)))))

  (testing "Vector with unresolved variable retains TVar"
    (let [var-node (n/->variable 'x {} {} nil)   ;; 未绑定
          lit-node (n/->literal 42 {} {} nil)
          vec-node (n/->vector [var-node lit-node] {} {} nil)
          typed-vec (run-solve vec-node {})
          vec-ty (ty/get-type typed-vec)]
      (is (instance? THeteroVec vec-ty))
      (let [[ty1 ty2] (:types vec-ty)]
        ;; x 没有约束，因此应该是 TVar
        (is (ty/var-type? ty1))
        ;; 42 是 int64
        (is (= :int64 (:name ty2)))))))
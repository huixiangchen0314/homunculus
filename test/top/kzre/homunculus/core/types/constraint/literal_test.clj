(ns top.kzre.homunculus.core.types.constraint.literal-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api]   ;; 注册所有多方法
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))

(defn- run-solve
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest literal-constraint-generation
  (testing "integer literal becomes int64"
    (let [lit-node (n/->literal 42 {} {} nil)
          typed-lit (run-solve lit-node {})
          lit-ty (ty/get-type typed-lit)]
      (is (= :int64 (ty/type-sym lit-ty)))))

  (testing "string literal becomes string"
    (let [lit-node (n/->literal "hello" {} {} nil)
          typed-lit (run-solve lit-node {})
          lit-ty (ty/get-type typed-lit)]
      (is (= :string (ty/type-sym lit-ty)))))

  (testing "boolean literal becomes bool"
    (let [lit-node (n/->literal false {} {} nil)
          typed-lit (run-solve lit-node {})
          lit-ty (ty/get-type typed-lit)]
      (is (= :bool (ty/type-sym lit-ty)))))

  (testing "nil literal becomes nil"
    (let [lit-node (n/->literal nil {} {} nil)
          typed-lit (run-solve lit-node {})
          lit-ty (ty/get-type typed-lit)]
      (is (= :nil (ty/type-sym lit-ty)))))

  (testing "float literal becomes float64"
    (let [lit-node (n/->literal 3.14 {} {} nil)
          typed-lit (run-solve lit-node {})
          lit-ty (ty/get-type typed-lit)]
      (is (= :float64 (ty/type-sym lit-ty))))))
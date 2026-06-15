(ns top.kzre.homunculus.core.types.constraint.try-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api]
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

(deftest try-constraint-generation
  (testing "try node gets a type variable (TVar)"
    (let [body-node (n/->literal 42 {} {} nil)
          try-node (n/->try [body-node] [] nil {} {} nil)
          typed-try (run-solve try-node {})
          try-ty (ty/get-type typed-try)]
      (is (some? try-ty))
      ;; try 不生成具体约束，所以类型保持 TVar（经泛化可能为 TVar 或 nil？这里预期 TVar）
      (is (ty/var-type? try-ty))))

  (testing "catch node gets a type variable"
    (let [catch-body (n/->literal "error" {} {} nil)
          catch-node (n/->catch 'Exception 'e catch-body {} {} nil)
          typed-catch (run-solve catch-node {})
          catch-ty (ty/get-type typed-catch)]
      (is (some? catch-ty))
      (is (ty/var-type? catch-ty))))

  (testing "throw node gets a type variable"
    (let [expr (n/->literal "boom" {} {} nil)
          throw-node (n/->throw expr {} {} nil)
          typed-throw (run-solve throw-node {})
          throw-ty (ty/get-type typed-throw)]
      (is (some? throw-ty))
      (is (ty/var-type? throw-ty)))))
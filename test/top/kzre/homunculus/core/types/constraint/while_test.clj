(ns top.kzre.homunculus.core.types.constraint.while-test
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

(deftest while-constraint-generation
  (testing "while node type is always nil"
    (let [test-node (n/->literal true {} {} nil)
          body-node (n/->literal 42 {} {} nil)
          while-node (n/->while test-node body-node {} {} nil)
          typed-while (run-solve while-node {})
          while-ty (ty/get-type typed-while)]
      (is (= nil (ty/type-sym while-ty)))))

  (testing "while with non-bool test still gives nil (bool constraint ignored)"
    (let [test-node (n/->literal 1 {} {} nil)     ;; int64, not bool
          body-node (n/->literal 42 {} {} nil)
          while-node (n/->while test-node body-node {} {} nil)
          typed-while (run-solve while-node {})
          while-ty (ty/get-type typed-while)]
      (is (= nil (ty/type-sym while-ty)))))

  #_(testing "body sub-node retains its inferred type (not overwritten to nil)"
    (let [test-node (n/->literal true {} {} nil)
          body-node (n/->literal "hello" {} {} nil)  ;; string
          while-node (n/->while test-node body-node {} {} nil)
          typed-while (run-solve while-node {})
          typed-body (n/while-body typed-while)
          body-ty (ty/get-type typed-body)]
      (is (= :string (ty/type-sym body-ty))))))
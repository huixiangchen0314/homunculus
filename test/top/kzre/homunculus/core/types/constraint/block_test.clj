(ns top.kzre.homunculus.core.types.constraint.block-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api]   ;; 加载所有 defmethod
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.type :as ty]))
;; ;;polymorphic
(def frontend (tu/->MockFrontend))

(defn- run-solve
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest block-constraint-generation
  (testing "block with multiple expressions — type is type of last expression"
    (let [expr1 (n/->literal 1 {} {} nil)
          expr2 (n/->literal "hello" {} {} nil)
          block-node (n/->block [expr1 expr2] {} {} nil)
          typed-block (run-solve block-node {})
          ret-ty (ty/get-type typed-block)]
      (is (some? ret-ty))
      (is (= :string (:name ret-ty)))))

  (testing "block with single expression — type is that expression's type"
    (let [expr (n/->literal 42 {} {} nil)
          block-node (n/->block [expr] {} {} nil)
          typed-block (run-solve block-node {})
          ret-ty (ty/get-type typed-block)]
      (is (= :int64 (:name ret-ty)))))

  (testing "empty block — type is nil"
    (let [block-node (n/->block [] {} {} nil)
          typed-block (run-solve block-node {})
          ret-ty (ty/get-type typed-block)]
      ;; nil 字面量对应的 TCon :nil
      (is (= :nil (:name ret-ty))))))
(ns top.kzre.homunculus.core.types.constraint.define-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api]   ;; 加载所有 defmethod
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

(deftest define-constraint-generation
  (testing "define node gets value type"
    (let [val-node (n/->literal 42 {} {} nil)
          define-node (n/->define 'answer val-node nil {} {} nil)
          typed-define (run-solve define-node {})
          ret-ty (ty/get-type typed-define)]
      (is (= :int64 (:name ret-ty)))))

  (testing "define with builtin function value keeps function type"
    (let [val-node (n/->variable 'inc {} {} nil)    ;; inc 是内置函数 int64->int64
          define-node (n/->define 'my-inc val-node nil {} {} nil)
          typed-define (run-solve define-node {})
          ret-ty (ty/get-type typed-define)]
      (is (ty/fun-type? ret-ty))
      (is (= :int64 (-> ret-ty :arg :name))))))
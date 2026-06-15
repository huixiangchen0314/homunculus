(ns top.kzre.homunculus.core.types.constraint.lambda-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api]   ;; 加载所有 defmethod
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))

(defn- run-solve
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest lambda-constraint-generation
  (testing "Simple lambda: parameter and return are same TVar (body returns param)"
    (let [param (n/->variable 'x {} {} nil)   ;; 无预标注
          body (n/->variable 'x {} {} nil)    ;; 直接返回参数
          lambda-node (n/->lambda [param] body nil nil {} {} nil)
          typed-lambda (run-solve lambda-node {})
          lambda-ty (ty/get-type typed-lambda)]
      (is (ty/fun-type? lambda-ty))
      (let [arg-ty (ty/fun-arg lambda-ty)
            ret-ty (ty/fun-ret lambda-ty)]
        (is (ty/var-type? arg-ty))
        ;; body 就是参数，因此参数与返回是同一个 TVar
        (is (= arg-ty ret-ty)))))

  (testing "Lambda with pre-annotated parameter type — parameter becomes concrete, return follows"
    (let [param (-> (n/->variable 'a {} {} nil) (ty/set-type! (t/->TCon :int64)))
          body (n/->variable 'a {} {} nil)
          lambda-node (n/->lambda [param] body nil nil {} {} nil)
          typed-lambda (run-solve lambda-node {})
          lambda-ty (ty/get-type typed-lambda)]
      (is (ty/fun-type? lambda-ty))
      (is (= :int64 (:name (ty/fun-arg lambda-ty))))
      (is (= :int64 (:name (ty/fun-ret lambda-ty))))))

  (testing "Lambda with body producing constant type — param TVar, ret concrete"
    (let [param (n/->variable 'x {} {} nil)
          body (n/->literal 42 {} {} nil)   ;; int64
          lambda-node (n/->lambda [param] body nil nil {} {} nil)
          typed-lambda (run-solve lambda-node {})
          lambda-ty (ty/get-type typed-lambda)]
      (is (ty/fun-type? lambda-ty))
      (is (ty/var-type? (ty/fun-arg lambda-ty)))
      (is (= :int64 (:name (ty/fun-ret lambda-ty)))))))
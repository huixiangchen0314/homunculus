(ns top.kzre.homunculus.core.types.constraint.let-test
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

(deftest let-constraint-generation
  (testing "let with single binding — body type inferred"
    (let [var-node (n/->variable 'x {} {} nil)
          val-node (n/->literal 42 {} {} nil)
          body-node (n/->variable 'x {} {} nil)
          let-node (n/->let [[var-node val-node]] body-node {} {} nil)
          typed-let (run-solve let-node {})
          ret-ty (ty/get-type typed-let)]
      (is (= :int64 (:name ret-ty)))))

  (testing "let with multiple bindings — body uses first binding"
    (let [var1 (n/->variable 'a {} {} nil)
          val1 (n/->literal 1 {} {} nil)
          var2 (n/->variable 'b {} {} nil)
          val2 (n/->literal true {} {} nil)
          body (n/->variable 'a {} {} nil)
          let-node (n/->let [[var1 val1] [var2 val2]] body {} {} nil)
          typed-let (run-solve let-node {})
          ret-ty (ty/get-type typed-let)]
      (is (= :int64 (:name ret-ty)))))

  (testing "let with function value generalizes to TScheme"
    (let [var-node (n/->variable 'f {} {} nil)
          ;; 内置函数 inc 作为值，类型为 int64 -> int64
          val-node (n/->variable 'inc {} {} nil)
          body-node (n/->variable 'f {} {} nil)
          let-node (n/->let [[var-node val-node]] body-node {} {} nil)
          typed-let (run-solve let-node {})
          ret-ty (ty/get-type typed-let)]
      ;; body 引用的 'f 应该被泛化为 TScheme，但实例化后得到具体类型
      (is (ty/fun-type? ret-ty))
      (is (= :int64 (-> ret-ty :arg :name))))))
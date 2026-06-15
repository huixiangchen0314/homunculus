(ns top.kzre.homunculus.core.types.constraint.variable-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.api]   ;; 注册所有多方法
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.types.test-utils :as tu]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))

(defn- run-solve
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest variable-constraint-generation
  (testing "variable found in environment gets its type"
    (let [env (e/extend-env {} 'x (t/->TCon :int64))
          var-node (n/->variable 'x {} {} nil)
          typed-var (run-solve var-node env)
          var-ty (ty/get-type typed-var)]
      (is (= :int64 (:name var-ty)))))

  (testing "variable not in environment but is a builtin function gets function type"
    ;; '+' 是 MockFrontend 中的内置函数，类型为 int64 -> int64 -> int64
    (let [var-node (n/->variable '+ {} {} nil)
          typed-var (run-solve var-node {})  ;; 空环境
          var-ty (ty/get-type typed-var)]
      (is (ty/fun-type? var-ty))
      (is (= :int64 (-> var-ty :arg :name)))))

  (testing "variable completely unknown gets a type variable (TVar)"
    (let [var-node (n/->variable 'unknown {} {} nil)
          typed-var (run-solve var-node {})
          var-ty (ty/get-type typed-var)]
      ;; 未知变量分配 TVar，求解后未被约束，仍为 TVar
      (is (ty/var-type? var-ty)))))
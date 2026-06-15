(ns top.kzre.homunculus.core.types.constraint.if-test
  (:require
    [clojure.test :refer :all]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.api]   ;; 注册所有多方法
    [top.kzre.homunculus.core.types.constraint.solve :as solve]
    [top.kzre.homunculus.core.types.protocol :as p]
    [top.kzre.homunculus.core.types.test-utils :as tu]
    [top.kzre.homunculus.core.types.type :as ty]))

(def frontend (tu/->MockFrontend))

(defn- run-solve
  "辅助：对单节点生成约束并求解，返回替换后的节点。"
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node]
                                                              {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    typed-root))

(deftest if-constraint-generation
  (testing "if with bool test and matching branches solves to branch type"
    (let [test-node (n/->literal true {} {} nil)
          then-node (n/->literal 1 {} {} nil)
          else-node (n/->literal 2 {} {} nil)
          if-node   (n/->if test-node then-node else-node {} {} nil)
          typed-if  (run-solve if-node {})
          ret-ty    (ty/get-type typed-if)]
      (is (some? ret-ty))
      (is (= :int64 (:name ret-ty)))))

  (testing "if without else branch solves to then type"
    (let [test-node (n/->literal true {} {} nil)
          then-node (n/->literal 42 {} {} nil)
          if-node   (n/->if test-node then-node nil {} {} nil)
          typed-if  (run-solve if-node {})
          ret-ty    (ty/get-type typed-if)]
      (is (= :int64 (:name ret-ty)))))

  (testing "if with non-bool test still infers then type (bool constraint ignored)"
    (let [test-node (n/->literal 1 {} {} nil)     ;; int64, not bool
          then-node (n/->literal 42 {} {} nil)
          if-node   (n/->if test-node then-node nil {} {} nil)
          typed-if  (run-solve if-node {})
          ret-ty    (ty/get-type typed-if)]
      ;; test 的 bool 约束因冲突被忽略，但 then 的约束仍然有效，因此整体类型为 int64
      (is (some? ret-ty))
      (is (= :int64 (:name ret-ty))))))
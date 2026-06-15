(ns top.kzre.homunculus.core.types.constraint.call-test
  (:require
   [clojure.test :refer :all]
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
   [top.kzre.homunculus.core.types.constraint.api]
   [top.kzre.homunculus.core.types.constraint.solve :as solve]
   [top.kzre.homunculus.core.types.protocol :as p]
   [top.kzre.homunculus.core.types.test-utils :as tu]
   [top.kzre.homunculus.core.types.type :as ty]))

;; 使用 Mock 前端（内置函数已包含 +、inc 等）
(def frontend (tu/->MockFrontend))

(defn- run-solve
  "辅助：对单节点生成约束并求解，返回替换后的节点和替换映射。"
  [node env]
  (let [{:keys [roots constraints]} (gen/generate-constraints [node] {:frontend frontend :env env})
        subst (solve/solve-constraints constraints)
        typed-root (first (mapv #(solve/apply-subst % subst) roots))]
    [typed-root subst]))

(deftest call-constraint-generation
  (let [env {} ; 空环境，函数类型由前端内置函数提供
        fn-node (n/->variable '+ {} {} nil)
        args [(n/->literal 1 {} {} nil)
              (n/->literal 2 {} {} nil)]
        call-node (n/->call fn-node args {} {} nil)]
    (testing "Call to builtin + generates equality constraints and solves to int64"
      (let [[typed-node subst] (run-solve call-node env)
            ret-ty (ty/get-type typed-node)]
        (is (some? ret-ty))
        (is (= :int64 (:name ret-ty)))))))


(deftest call-with-unresolved-function
  (let [env {}
        fn-node (n/->variable 'unknown-fn {} {} nil) ; 非内置，环境无绑定
        args [(n/->literal 1 {} {} nil)]
        call-node (n/->call fn-node args {} {} nil)]
    (testing "Unresolved function leaves type variable or nil"
      (let [[typed-node subst] (run-solve call-node env)
            ret-ty (ty/get-type typed-node)]
        ;; 实际行为：如果函数类型完全未知，可能分配一个 TVar 并保留（泛化后为 scheme）
        ;; 或直接为 nil。这里只检查不会崩溃。
        (is (or (nil? ret-ty) (satisfies? p/IType ret-ty)))))))
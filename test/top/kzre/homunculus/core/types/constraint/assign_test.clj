(ns top.kzre.homunculus.core.types.constraint.assign-test
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

(deftest assign-constraint-generation
  (testing "assign with matching types — var and val types are unified, assign type is nil"
    (let [var-node (n/->variable 'x {} {} nil)   ;; 变量 'x
          val-node (n/->literal 42 {} {} nil)    ;; int64
          assign-node (n/->assign var-node val-node {} {} nil)
          typed-assign (run-solve assign-node {})
          assign-ty (ty/get-type typed-assign)
          ;; 从 typed-assign 中取出 var 和 val 的类型
          typed-var (n/assign-var typed-assign)
          typed-val (n/assign-val typed-assign)
          var-ty (ty/get-type typed-var)
          val-ty (ty/get-type typed-val)]
      ;; assign 本身类型固定为 nil
      (is (= :nil (ty/type-sym assign-ty)))
      ;; var 和 val 的类型被统一为 int64
      (is (= :int64 (ty/type-sym var-ty)))
      (is (= :int64 (ty/type-sym val-ty)))))

  (testing "assign with mismatched types — constraint ignored, types remain distinct"
    (let [var-node (n/->literal true {} {} nil)   ;; bool 作为被赋值的“变量”（测试用）
          val-node (n/->literal 42 {} {} nil)     ;; int64
          assign-node (n/->assign var-node val-node {} {} nil)
          typed-assign (run-solve assign-node {})
          typed-var (n/assign-var typed-assign)
          typed-val (n/assign-val typed-assign)
          var-ty (ty/get-type typed-var)
          val-ty (ty/get-type typed-val)]
      ;; assign 类型仍为 nil
      (is (= :nil (ty/type-sym (ty/get-type typed-assign))))
      ;; 类型因冲突被忽略，保持原样：var 是 bool，val 是 int64
      (is (= :bool (ty/type-sym var-ty)))
      (is (= :int64 (ty/type-sym val-ty))))))
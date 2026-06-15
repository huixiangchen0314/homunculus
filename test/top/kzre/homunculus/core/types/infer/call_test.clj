(ns top.kzre.homunculus.core.types.infer.call-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.api]                   ;; 确保所有方法加载
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest call-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]   ;; 环境为空，依赖前端内置函数
    (testing "call to builtin + with two ints infers int64"
      (let [fn-node (n/->variable '+ {} {} nil)
            args [(n/->literal 1 {} {} nil) (n/->literal 2 {} {} nil)]
            call-node (n/->call fn-node args {} {} nil)
            [ty new-node] (infer/local-infer call-node context)]
        (is (some? ty))
        (is (= :int64 (:name ty)))))
    (testing "call to builtin inc with one int infers int64"
      (let [fn-node (n/->variable 'inc {} {} nil)
            args [(n/->literal 42 {} {} nil)]
            call-node (n/->call fn-node args {} {} nil)
            [ty new-node] (infer/local-infer call-node context)]
        (is (= :int64 (:name ty)))))))
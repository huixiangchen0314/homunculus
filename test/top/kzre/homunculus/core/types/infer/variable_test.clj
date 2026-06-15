(ns top.kzre.homunculus.core.types.infer.variable-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.variable]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest variable-inference
  (let [env (e/extend-env {} 'x (t/->TCon :int64))
        context {:env env}]
    (testing "bound variable returns its type from env"
      (let [node (n/->variable 'x {} {} nil)
            [ty new-node] (infer/local-infer node context)]
        (is (= :int64 (:name ty)))))
    (testing "unbound variable returns nil type"
      (let [node (n/->variable 'y {} {} nil)
            [ty new-node] (infer/local-infer node {:env {}})]
        (is (nil? ty))))))
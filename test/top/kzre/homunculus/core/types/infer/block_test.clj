(ns top.kzre.homunculus.core.types.infer.block-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods.block]
            [top.kzre.homunculus.core.types.infer.methods.literal]
            [top.kzre.homunculus.core.types.test-utils :as tu]))

(deftest block-inference
  (let [frontend (tu/->MockFrontend)
        context {:frontend frontend :env {}}]
    (testing "block type is type of last expression"
      (let [exprs [(n/->literal 1 {} {} nil)
                   (n/->literal "end" {} {} nil)]
            block (n/->block exprs {} {} nil)
            [ty new-node] (infer/local-infer block context)]
        (is (= :string (:name ty)))))
    (testing "empty block returns nil"
      (let [block (n/->block [] {} {} nil)
            [ty new-node] (infer/local-infer block context)]
        (is (nil? ty))))))
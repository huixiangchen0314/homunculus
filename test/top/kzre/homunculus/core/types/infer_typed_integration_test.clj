(ns top.kzre.homunculus.core.types.infer-typed-integration-test
  "测试 infer‑pass → typed‑pass 的串联效果。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.constraint.solve :as cs]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]
    ;; 注册 infer 多方法
            [top.kzre.homunculus.core.types.test-utils :refer :all] ;; 注册 typed 多方法
            [top.kzre.homunculus.core.types.typed.methods]))


(deftest let-binding-infer-typed-chain
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 42 nil nil nil)
        var-node (m/->VariableNode "x" nil nil nil)
        body-node (m/->VariableNode "x" nil nil nil)
        let-node (m/->LetNode [[var-node val-node]] body-node nil nil nil)
        infer-result (first (infer/run [let-node] :frontend frontend))
        typed-results (cs/process [infer-result] {:frontend frontend :env {}})
        final-node (first typed-results)]
    (testing "最终类型应为 int64"
      (is (tcon? (-> final-node ir2p/attrs :type) :int64))
      (let [body-x (-> final-node :body)]
        (is (tcon? (-> body-x ir2p/attrs :type) :int64))))))
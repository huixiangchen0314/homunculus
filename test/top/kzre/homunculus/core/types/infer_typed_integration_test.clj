(ns top.kzre.homunculus.core.types.infer-typed-integration-test
  "测试 infer‑pass → typed‑pass 的串联效果。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.infer.methods]   ;; 注册 infer 多方法
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]   ;; 注册 typed 多方法
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))


(deftest let-binding-infer-typed-chain
  (let [frontend (->MockFrontend)
        ;; 构造 IR2 节点：(let [x 42] x)
        val-node (m/->LiteralNode 42 nil nil nil)
        var-node (m/->VariableNode "x" nil nil  nil)
        body-node (m/->VariableNode "x" nil nil  nil)
        let-node (m/->LetNode [[var-node val-node]] body-node nil nil  nil)

        ;; 第一步：infer‑pass 推导
        infer-result (first (infer/run [let-node] :frontend frontend))

        ;; 第二步：typed‑pass 推导（复用 infer 结果）
        typed-results (typed/type-check [infer-result] :frontend frontend)
        final-node (first typed-results)]
    (testing "最终类型应为 int64"
      (is (tcon? (-> final-node ir2p/attrs :type) :int64))
      ;; body 变量 x 的类型也应是 int64，并且来自 infer（不需重新推导）
      (let [body-x (-> final-node :body)]
        (is (tcon? (-> body-x ir2p/attrs :type) :int64))))))
(ns top.kzre.homunculus.core.types.typed.integration-test
  "infer → typed → check 串联集成测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.methods]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(deftest let-binding-infer-typed-check
  (let [frontend (->MockFrontend)
        backend  (->MockBackend)

        val-node (m/->LiteralNode 42 nil nil nil)
        var-node (m/->VariableNode "x" nil nil nil)
        body-node (m/->VariableNode "x" nil nil nil)
        let-node (m/->LetNode [[var-node val-node]] body-node nil nil nil)

        ;; infer
        infer-result (first (infer/infer [let-node] :frontend frontend))
        ;; typed
        typed-result (first (typed/type-check [infer-result] :frontend frontend))
        ;; check with expected float32 on the whole let
        checked (check/check typed-result (t/->TCon :float32) {:backend backend})]

    ;; 目前 let 节点的整体类型尚未被 check 转换为期望类型，仍为 int64
    (is (tcon? (get-type checked) :int64) "let 整体类型暂时为 int64")

    (let [body (-> checked :body)]
      (is (convert? body) "body 变量 x 应被转换为 float32")
      (is (tcon? (get-type body) :float32) "转换后 body 类型应为 float32"))))

(deftest literal-infer-typed-check
  (let [frontend (->MockFrontend)
        backend  (->MockBackend)

        lit-node (m/->LiteralNode 42 nil nil nil)

        ;; infer
        infer-result (first (infer/infer [lit-node] :frontend frontend))
        ;; typed
        typed-result (first (typed/type-check [infer-result] :frontend frontend))
        ;; check with expected float32
        checked (check/check typed-result (t/->TCon :float32) {:backend backend})]

    (is (convert? checked) "字面量应被包装为 convert")
    (is (tcon? (get-type checked) :float32) "转换后类型应为 float32")))
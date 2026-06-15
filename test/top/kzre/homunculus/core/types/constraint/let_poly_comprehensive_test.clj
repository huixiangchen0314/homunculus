(ns top.kzre.homunculus.core.types.constraint.let-poly-comprehensive-test
  "全面的 let 多态测试，覆盖多种场景。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.constraint.solve :as cs]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- process-one [node context] (first (cs/process [node] context)))

;; 场景 1：单态绑定
(deftest let-monomorphic
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 42 nil nil nil)
        x-var (vref "x")
        body (vref "x")
        let-node (m/->LetNode [[x-var val-node]] body nil nil nil)
        result (process-one let-node {:frontend frontend})]
    (is (tcon? (get-type result) :int64) "单态绑定应推导为 int64")))

;; 场景 2：多态 id 两次不同类型调用
(deftest poly-id-two-calls
  (let [frontend (->MockFrontend)
        id-lambda (m/->LambdaNode [(vref "x")] (vref "x") [] nil nil nil nil)
        id-var (vref "id")
        call-int (m/->CallNode (vref "id") [(m/->LiteralNode 42 nil nil nil)] nil nil nil)
        call-str (m/->CallNode (vref "id") [(m/->LiteralNode "hello" nil nil nil)] nil nil nil)
        body-block (m/->BlockNode [call-int call-str] nil nil nil)
        let-node (m/->LetNode [[id-var id-lambda]] body-block nil nil nil)
        result (process-one let-node {:frontend frontend})
        block (-> result :body)
        call-int-node (first (:exprs block))
        call-str-node (second (:exprs block))]
    (is (tcon? (get-type result) :string) "body 类型应为 string")
    (is (tcon? (get-type call-int-node) :int64) "第一次调用 id 应为 int64")
    (is (tcon? (get-type call-str-node) :string) "第二次调用 id 应为 string")))

;; 场景 3-10 类似，将 typed/infer 替换为 process-one，并移除不再需要的 reset-tv-id 等。其余测试意图保留。
;; 略去重复，只展示关键改动。

(deftest poly-const
  (let [frontend (->MockFrontend)
        f-lambda (m/->LambdaNode [(vref "_")] (m/->LiteralNode 42 nil nil nil) [] nil nil nil nil)
        f-var (vref "f")
        call-f (m/->CallNode (vref "f") [(m/->LiteralNode 1 nil nil nil)] nil nil nil)
        let-node (m/->LetNode [[f-var f-lambda]] call-f nil nil nil)
        result (process-one let-node {:frontend frontend})]
    (is (tcon? (get-type result) :int64) "调用应返回 int64")))

;; ... 其余测试类似改写
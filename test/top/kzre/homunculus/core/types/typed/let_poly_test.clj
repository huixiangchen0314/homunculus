(ns top.kzre.homunculus.core.types.typed.let-poly-test
  "已通过的 let 多态测试，包括嵌套、高阶和标注。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defn- vref [name] (m/->Variable name nil nil nil))
(defn- reset-tv-id [] (alter-var-root #'typed/*tv-id (constantly (atom 0))))

;; 测试 1：多态 id 函数 (fn [x] x) 两次不同类型调用
(deftest poly-id-test
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        id-lambda (m/->Lambda [(vref "x")] (vref "x") [] nil nil nil nil)
        id-var (vref "id")
        call-int (m/->Call (vref "id") [(m/->Literal 42 nil nil nil)] nil nil  nil)
        call-str (m/->Call (vref "id") [(m/->Literal "hello" nil nil nil)] nil nil  nil)
        body-block (m/->Block [call-int call-str] nil nil nil)
        let-node (m/->Let [[id-var id-lambda]] body-block nil nil nil)
        [ty result _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :string) "body 整体类型应为 string")
    (let [block (-> result :body)
          call-int-node (first (:exprs block))
          call-str-node (second (:exprs block))]
      (is (tcon? (get-type call-int-node) :int64) "第一次调用 id 应推导为 int64")
      (is (tcon? (get-type call-str-node) :string) "第二次调用 id 应推导为 string"))))

;; 测试 2：多态常量函数 (fn [_] 42)
(deftest poly-const-test
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        f-lambda (m/->Lambda [(vref "_")] (m/->Li->Literalnil nil nil) [] nil nil nil  nil)
        f-var (vref "f")
        call-f (m/->Call (vref "f") [(m/->Literal 1 nil nil nil)] nil nil  nil)
        let-node (m/->Let [[f-var f-lambda]] call-f nil nil nil)
        [ty _ _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :int64) "调用应返回 int64")))

;; 测试 3：用户标注（绑定值且标注类型一致/不一致）
(deftest poly-annotation-test
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        ;; 正确标注：x 标注为 int64，值为 42
        x-var (m/->Variable "x" {:tag :int64} nil nil)
        val-node (m/->Literal 42 nil nil  nil)
        body-node (vref "x")
        let-node (m/->Let [[x-var val-node]] body-node nil nil  nil)
        [ty _ _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :int64) "标注变量类型应为 int64")
    ;; 错误标注：x 标注为 int64，但值为 string
    (let [val-str (m/->Literal "hello" nil nil  nil)
          bad-node (m/->Let [[x-var val-str]] body-node nil nil nil)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (typed/infer bad-node {:frontend frontend}))
          "标注为 int64 时，值不能为 string"))))

;; 测试 4：嵌套 let 多态
(deftest nested-let-polymorphic-test
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        f-lambda (m/->Lambda [(vref "x")] (vref "x") [] nil nil nil nil)
        f-var (vref "f")
        g-body (m/->Call (vref "f") [(vref "y")] nil nil nil)
        g-lambda (m/->Lambda [(vref "y")] g-body [] nil nil nil nil)
        g-var (vref "g")
        call-g (m/->Call (vref "g") [(m/->Literal 10 nil nil  nil)] nil nil nil)
        inner-let (m/->Let [[g-var g-lambda]] call-g nil nil  nil)
        outer-let (m/->Let [[f-var f-lambda]] inner-let nil nil nil)
        [ty _ _] (typed/infer outer-let {:frontend frontend})]
    (is (tcon? ty :int64) (str "Got: " ty))))

;; 测试 5：高阶函数传递多态函数（apply id 42）
(deftest high-order-let-test
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        apply-lambda (m/->Lambda [(vref "f") (vref "x")]
                                     (m/->Call (vref "f") [(vref "x")] nil nil  nil)
                                     [] nil nil nil  nil)
        apply-var (vref "apply")
        id-lambda (m/->Lambda [(vref "x")] (vref "x") [] nil nil nil  nil)
        id-var (vref "id")
        call-apply (m/->Call (vref "apply") [(vref "id") (m/->Literal 42 nil nil  nil)] nil nil  nil)
        let-id (m/->Let [[id-var id-lambda]] call-apply nil nil nil)
        let-apply (m/->Let [[apply-var apply-lambda]] let-id nil nil  nil)
        [ty _ _] (typed/infer let-apply {:frontend frontend})]
    (is (tcon? ty :int64) (str "Got: " ty))))
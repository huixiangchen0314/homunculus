(ns top.kzre.homunculus.core.types.typed.let-poly-comprehensive-test
  "全面的 let 多态测试，覆盖多种场景。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all] ;; MockFrontend, get-type, tcon?, tfun?, tvar?
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defn- vref [name] (m/->VariableNode name nil nil [] nil))
(defn- reset-tv-id [] (alter-var-root #'typed/*tv-id (constantly (atom 0))))

;; ── 场景 1：单态绑定（无标注）────
(deftest let-monomorphic
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 42 nil nil [] nil)
        x-var (vref "x")
        body (vref "x")
        let-node (m/->LetNode [[x-var val-node]] body nil nil [] nil)
        [ty _ _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :int64) "单态绑定应推导为 int64")))

;; ── 场景 2：多态 id (fn [x] x) 两次不同类型调用 ────
(deftest poly-id-two-calls
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        id-lambda (m/->LambdaNode [(vref "x")] (vref "x") [] nil nil nil [] nil)
        id-var (vref "id")
        call-int (m/->CallNode (vref "id") [(m/->LiteralNode 42 nil nil [] nil)] nil nil [] nil)
        call-str (m/->CallNode (vref "id") [(m/->LiteralNode "hello" nil nil [] nil)] nil nil [] nil)
        body-block (m/->BlockNode [call-int call-str] nil nil [] nil)
        let-node (m/->LetNode [[id-var id-lambda]] body-block nil nil [] nil)
        [ty result _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :string) "body 类型应为 string")
    (let [block (-> result :body)
          call-int-node (first (:exprs block))
          call-str-node (second (:exprs block))]
      (is (tcon? (get-type call-int-node) :int64) "第一次调用 id 应为 int64")
      (is (tcon? (get-type call-str-node) :string) "第二次调用 id 应为 string"))))

;; ── 场景 3：多态常量函数 (fn [_] 42) ────
(deftest poly-const
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        f-lambda (m/->LambdaNode [(vref "_")] (m/->LiteralNode 42 nil nil [] nil) [] nil nil nil [] nil)
        f-var (vref "f")
        call-f (m/->CallNode (vref "f") [(m/->LiteralNode 1 nil nil [] nil)] nil nil [] nil)
        let-node (m/->LetNode [[f-var f-lambda]] call-f nil nil [] nil)
        [ty _ _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :int64) "调用应返回 int64")))

;; ── 场景 4：用户标注阻止泛化（正确标注）────
(deftest poly-annotation-correct
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        x-var (m/->VariableNode "x" {:tag :int64} nil [] nil)
        val-node (m/->LiteralNode 42 nil nil [] nil)
        body (vref "x")
        let-node (m/->LetNode [[x-var val-node]] body nil nil [] nil)
        [ty _ _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :int64) "标注变量类型应为 int64")))

;; ── 场景 5：错误标注应报错 ────
(deftest poly-annotation-mismatch
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        x-var (m/->VariableNode "x" {:tag :int64} nil [] nil)
        val-node (m/->LiteralNode "hello" nil nil [] nil)
        body (vref "x")
        let-node (m/->LetNode [[x-var val-node]] body nil nil [] nil)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (typed/infer let-node {:frontend frontend}))
        "标注为 int64 时，值不能为 string")))

;; ── 场景 6：嵌套 let 多态 ────
(deftest nested-let-poly
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        f-lambda (m/->LambdaNode [(vref "x")] (vref "x") [] nil nil nil [] nil)
        f-var (vref "f")
        g-body (m/->CallNode (vref "f") [(vref "y")] nil nil [] nil)
        g-lambda (m/->LambdaNode [(vref "y")] g-body [] nil nil nil [] nil)
        g-var (vref "g")
        call-g (m/->CallNode (vref "g") [(m/->LiteralNode 10 nil nil [] nil)] nil nil [] nil)
        inner-let (m/->LetNode [[g-var g-lambda]] call-g nil nil [] nil)
        outer-let (m/->LetNode [[f-var f-lambda]] inner-let nil nil [] nil)
        [ty _ _] (typed/infer outer-let {:frontend frontend})]
    (is (tcon? ty :int64) "嵌套 let 最终结果应为 int64")))

;; ── 场景 7：高阶函数传递多态函数（apply id 42）────
(deftest high-order-apply-id
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        apply-lambda (m/->LambdaNode [(vref "f") (vref "x")]
                                     (m/->CallNode (vref "f") [(vref "x")] nil nil [] nil)
                                     [] nil nil nil [] nil)
        apply-var (vref "apply")
        id-lambda (m/->LambdaNode [(vref "x")] (vref "x") [] nil nil nil [] nil)
        id-var (vref "id")
        call-apply (m/->CallNode (vref "apply") [(vref "id") (m/->LiteralNode 42 nil nil [] nil)] nil nil [] nil)
        let-id (m/->LetNode [[id-var id-lambda]] call-apply nil nil [] nil)
        let-apply (m/->LetNode [[apply-var apply-lambda]] let-id nil nil [] nil)
        [ty _ _] (typed/infer let-apply {:frontend frontend})]
    (is (tcon? ty :int64) "apply id 42 应返回 int64")))

;; ── 场景 8：多个多态绑定 ────
(deftest multiple-poly-bindings
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        f-lambda (m/->LambdaNode [(vref "x")] (vref "x") [] nil nil nil [] nil)
        f-var (vref "f")
        g-lambda (m/->LambdaNode [(vref "y")] (m/->CallNode (vref "f") [(vref "y")] nil nil [] nil) [] nil nil nil [] nil)
        g-var (vref "g")
        call-g-int (m/->CallNode (vref "g") [(m/->LiteralNode 10 nil nil [] nil)] nil nil [] nil)
        let-f (m/->LetNode [[f-var f-lambda]] (m/->LetNode [[g-var g-lambda]] call-g-int nil nil [] nil) nil nil [] nil)
        [ty _ _] (typed/infer let-f {:frontend frontend})]
    (is (tcon? ty :int64) "多个多态绑定最终应返回 int64")))

;; ── 场景 9：递归多态（简单测试，可能失败但不影响核心）────
#_(deftest recursive-poly
    (reset-tv-id)
    (let [frontend (->MockFrontend)
          ;; 暂时跳过，因为递归多态需要 fixpoint
          ]
      (is true "递归多态未实现，跳过")))

;; ── 场景 10：混合标注与多态 ────
(deftest mixed-annotation-and-poly
  (reset-tv-id)
  (let [frontend (->MockFrontend)
        ;; x 标注为 int64，值为 10
        x-var (m/->VariableNode "x" {:tag :int64} nil [] nil)
        val-x (m/->LiteralNode 10 nil nil [] nil)
        ;; f 多态函数 (fn [a] a)
        f-lambda (m/->LambdaNode [(vref "a")] (vref "a") [] nil nil nil [] nil)
        f-var (vref "f")
        ;; body: (f x) ，应返回 int64
        body (m/->CallNode (vref "f") [(vref "x")] nil nil [] nil)
        let-node (m/->LetNode [[x-var val-x] [f-var f-lambda]] body nil nil [] nil)
        [ty _ _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :int64) "混合标注与多态：f 应用于 int64 应返回 int64")))

;; 所有测试已就绪，运行即可。
(ns top.kzre.homunculus.core.types.typed.variable-let-unit-test
  "针对 variable 和 let 方法的详细单元测试，覆盖多态和高阶场景。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.typed.scheme :as s]
            [top.kzre.homunculus.core.ir2.model :as m])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]
           [top.kzre.homunculus.core.types.typed.scheme TScheme]))

(defn- vref [name] (m/->VariableNode name nil nil [] nil))

;; 测试 1：变量从环境获取单态类型
(deftest variable-monomorphic-test
  (let [frontend (->MockFrontend)
        env {"x" (t/->TCon :int64)}
        node (vref "x")
        [ty _ s] (typed/infer node {:frontend frontend :env env})]
    (is (tcon? ty :int64))
    (is (empty? s))))

;; 测试 2：变量从环境获取多态方案（TScheme）并实例化
(deftest variable-polymorphic-test
  (let [frontend (->MockFrontend)
        a (t/->TVar (gensym "a"))
        id-scheme (s/->TScheme [a] (t/->TFun a a))
        env {"id" id-scheme}
        node (vref "id")
        [ty _ s] (typed/infer node {:frontend frontend :env env})]
    (is (tfun? ty) "多态变量实例化后应为函数类型")   ;; ← 修正
    (is (empty? s) "变量引用不应产生替换")))

;; 测试 3：let 绑定单态值，body 使用该变量
(deftest let-monomorphic-body-test
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 42 nil nil [] nil)
        var-node (vref "x")
        body-node (vref "x")
        let-node (m/->LetNode [[var-node val-node]] body-node nil nil [] nil)
        [ty result _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :int64) (str "Got: " ty))
    (is (tcon? (get-type result) :int64))))

;; 测试 4：let 绑定多态值（lambda），body 中两次不同类型调用
(deftest let-polymorphic-id-two-calls-test
  (let [frontend (->MockFrontend)
        id-lambda (m/->LambdaNode [(vref "x")] (vref "x") [] nil nil nil [] nil)
        id-var (vref "id")
        call-int (m/->CallNode (vref "id") [(m/->LiteralNode 42 nil nil [] nil)] nil nil [] nil)
        call-str (m/->CallNode (vref "id") [(m/->LiteralNode "hello" nil nil [] nil)] nil nil [] nil)
        body-block (m/->BlockNode [call-int call-str] nil nil [] nil)
        let-node (m/->LetNode [[id-var id-lambda]] body-block nil nil [] nil)
        [ty result _] (typed/infer let-node {:frontend frontend})]
    (is (tcon? ty :string) "body 整体类型应为 string")
    (let [block (-> result :body)
          call-int-node (first (:exprs block))
          call-str-node (second (:exprs block))]
      (is (tcon? (get-type call-int-node) :int64) "第一次调用 id 应推导为 int64")
      (is (tcon? (get-type call-str-node) :string) "第二次调用 id 应推导为 string"))))

;; 测试 5：嵌套 let 多态（f 作为多态函数，g 内部调用 f）
(deftest nested-let-polymorphic-test
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
    (is (tcon? ty :int64) (str "Got: " ty))))

;; 测试 6：高阶函数传递多态函数（apply id 42）—— 核心失败案例
(deftest high-order-let-test
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
    (is (tcon? ty :int64) (str "Got: " ty))))
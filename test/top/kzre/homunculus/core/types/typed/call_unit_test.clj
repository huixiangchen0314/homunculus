(ns top.kzre.homunculus.core.types.typed.call-unit-test
  "针对 call 方法的详细单元测试，涵盖之前集成测试失败的高阶等案例。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.typed.scheme :as s]   ;; ← 关键：引入 scheme
            [top.kzre.homunculus.core.ir2.model :as m])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]
           [top.kzre.homunculus.core.types.typed.scheme TScheme]))

(defn- vref [name] (m/->VariableNode name nil nil [] nil))

;; 1. 已知单参函数 (int -> int)
(deftest call-known-tfun-one-arg-test
  (let [frontend (->MockFrontend)
        fn-ty (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
        fn-var (vref "f")
        arg-node (m/->LiteralNode 42 nil nil [] nil)
        call-node (m/->CallNode fn-var [arg-node] nil nil [] nil)
        env {"f" fn-ty}
        [ret-ty result _] (typed/infer call-node {:frontend frontend :env env})]
    (is (tcon? ret-ty :int64) (str "Got: " ret-ty))
    (is (tcon? (get-type result) :int64))))

;; 2. 已知多参函数 (int -> int -> int)
(deftest call-known-tfun-two-args-test
  (let [frontend (->MockFrontend)
        fn-ty (t/->TFun (t/->TCon :int64)
                        (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
        fn-var (vref "add")
        arg1 (m/->LiteralNode 1 nil nil [] nil)
        arg2 (m/->LiteralNode 2 nil nil [] nil)
        call-node (m/->CallNode fn-var [arg1 arg2] nil nil [] nil)
        env {"add" fn-ty}
        [ret-ty _ _] (typed/infer call-node {:frontend frontend :env env})]
    (is (tcon? ret-ty :int64) (str "Got: " ret-ty))))

;; 3. 函数类型为 TVar（未知），通过期望类型统一
(deftest call-unknown-tvar-test
  (let [frontend (->MockFrontend)
        fn-var (vref "g")
        arg-node (m/->LiteralNode 42 nil nil [] nil)
        call-node (m/->CallNode fn-var [arg-node] nil nil [] nil)
        env {"g" (t/->TVar (gensym "g"))}
        [ret-ty _ s] (typed/infer call-node {:frontend frontend :env env})]
    (is (tvar? ret-ty) "返回类型应为类型变量（尚未确定）")
    (is (seq s) "替换应包含 g 的类型映射")))

;; 4. 高阶调用：apply id 42  (之前集成测试失败的案例)
(deftest apply-id-call-test
  (let [frontend (->MockFrontend)
        a (t/->TVar (gensym "a"))
        b (t/->TVar (gensym "b"))
        apply-ty (t/->TFun (t/->TFun a b) (t/->TFun a b))
        apply-var (vref "apply")
        id-ty (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
        id-var (vref "id")
        arg-val (m/->LiteralNode 42 nil nil [] nil)
        call-node (m/->CallNode apply-var [id-var arg-val] nil nil [] nil)
        env {"apply" apply-ty "id" id-ty}
        [ret-ty _ _] (typed/infer call-node {:frontend frontend :env env})]
    (is (tcon? ret-ty :int64) (str "Got: " ret-ty))))

;; 5. 多态 id 两次不同类型调用（已知环境，对应 let-poly-test 中失败的高阶场景）
(deftest poly-id-two-calls-in-known-env-test
  (let [frontend (->MockFrontend)
        a (t/->TVar (gensym "a"))
        id-scheme (s/->TScheme [a] (t/->TFun a a))   ;; ← 使用 s/->TScheme
        env {"id" id-scheme}
        call1 (m/->CallNode (vref "id") [(m/->LiteralNode 42 nil nil [] nil)] nil nil [] nil)
        [ty1 _ _] (typed/infer call1 {:frontend frontend :env env})
        call2 (m/->CallNode (vref "id") [(m/->LiteralNode "hello" nil nil [] nil)] nil nil [] nil)
        [ty2 _ _] (typed/infer call2 {:frontend frontend :env env})]
    (is (tcon? ty1 :int64) "id 42 应为 int64")
    (is (tcon? ty2 :string) "id hello 应为 string")))

;; 6. 高阶 + 多态传递（已知环境）
(deftest nested-high-order-known-env-test
  (let [frontend (->MockFrontend)
        a (t/->TVar (gensym "a"))
        f-scheme (s/->TScheme [a] (t/->TFun a a))   ;; ← s/
        env {"f" f-scheme}
        call-inner (m/->CallNode (vref "f") [(m/->LiteralNode 42 nil nil [] nil)] nil nil [] nil)
        [inner-ty _ _] (typed/infer call-inner {:frontend frontend :env env})]
    (is (tcon? inner-ty :int64) "f 42 应为 int64")
    (let [env2 (assoc env "inner" inner-ty)
          call-outer (m/->CallNode (vref "f") [(vref "inner")] nil nil [] nil)
          [outer-ty _ _] (typed/infer call-outer {:frontend frontend :env env2})]
      (is (tcon? outer-ty :int64) "f (f 42) 应为 int64"))))
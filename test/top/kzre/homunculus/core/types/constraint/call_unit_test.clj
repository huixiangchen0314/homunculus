(ns top.kzre.homunculus.core.types.constraint.call-unit-test
  "针对 call 的约束生成和求解测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.constraint.solve :as cs]
            [top.kzre.homunculus.core.types.constraint.gen :as gen]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.ir2.model :as m])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defn- vref [name] (m/->VariableNode name nil nil nil))

(defn- solve-node [node context]
  (first (cs/process [node] context)))

;; 1. 已知单参函数 (int -> int)
(deftest call-known-tfun-one-arg-test
  (let [frontend (->MockFrontend)
        fn-ty (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
        fn-var (vref "f")
        arg-node (m/->LiteralNode 42 nil nil nil)
        call-node (m/->CallNode fn-var [arg-node] nil nil nil)
        result (solve-node call-node {:frontend frontend :env {"f" fn-ty}})
        ret-ty (get-type result)]
    (is (tcon? ret-ty :int64) (str "Got: " ret-ty))))

;; 2. 已知多参函数 (int -> int -> int)
(deftest call-known-tfun-two-args-test
  (let [frontend (->MockFrontend)
        fn-ty (t/->TFun (t/->TCon :int64)
                        (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
        fn-var (vref "add")
        arg1 (m/->LiteralNode 1 nil nil nil)
        arg2 (m/->LiteralNode 2 nil nil nil)
        call-node (m/->CallNode fn-var [arg1 arg2] nil nil nil)
        result (solve-node call-node {:frontend frontend :env {"add" fn-ty}})
        ret-ty (get-type result)]
    (is (tcon? ret-ty :int64) (str "Got: " ret-ty))))

;; 3. 函数类型为 TVar（未知），约束系统应分配 TVar
(deftest call-unknown-tvar-test
  (let [frontend (->MockFrontend)
        fn-var (vref "g")
        arg-node (m/->LiteralNode 42 nil nil nil)
        call-node (m/->CallNode fn-var [arg-node] nil nil nil)
        result (solve-node call-node {:frontend frontend :env {"g" (t/->TVar (gensym "g"))}})
        ret-ty (get-type result)]
    ;; 约束系统目前不做高阶推断，若无具体函数类型，返回类型将为 TVar
    (is (tvar? ret-ty) "返回类型应为类型变量（尚未确定）")))

;; 4. 高阶调用：apply id 42
(deftest apply-id-call-test
  (let [frontend (->MockFrontend)
        apply-ty (t/->TFun (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
                           (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
        apply-var (vref "apply")
        id-ty (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
        id-var (vref "id")
        arg-val (m/->LiteralNode 42 nil nil nil)
        call-node (m/->CallNode apply-var [id-var arg-val] nil nil nil)
        result (solve-node call-node {:frontend frontend :env {"apply" apply-ty "id" id-ty}})
        ret-ty (get-type result)]
    (is (tcon? ret-ty :int64) (str "Got: " ret-ty))))

;; 5. 多态 id 两次不同类型调用（TScheme 环境）
(deftest poly-id-two-calls-in-known-env-test
  (let [frontend (->MockFrontend)
        a (t/->TVar (gensym "a"))
        id-scheme (scheme/->TScheme [a] (t/->TFun a a))
        env {"id" id-scheme}
        call1 (m/->CallNode (vref "id") [(m/->LiteralNode 42 nil nil nil)] nil nil nil)
        r1 (solve-node call1 {:frontend frontend :env env})
        call2 (m/->CallNode (vref "id") [(m/->LiteralNode "hello" nil nil nil)] nil nil nil)
        r2 (solve-node call2 {:frontend frontend :env env})]
    (is (tcon? (get-type r1) :int64) "id 42 应为 int64")
    (is (tcon? (get-type r2) :string) "id hello 应为 string")))

;; 6. 高阶 + 多态传递（已知环境）
(deftest nested-high-order-known-env-test
  (let [frontend (->MockFrontend)
        a (t/->TVar (gensym "a"))
        f-scheme (scheme/->TScheme [a] (t/->TFun a a))
        env {"f" f-scheme}
        call-inner (m/->CallNode (vref "f") [(m/->LiteralNode 42 nil nil nil)] nil nil nil)
        r-inner (solve-node call-inner {:frontend frontend :env env})
        inner-ty (get-type r-inner)]
    (is (tcon? inner-ty :int64) "f 42 应为 int64")
    (let [env2 (assoc env "inner" inner-ty)
          call-outer (m/->CallNode (vref "f") [(vref "inner")] nil nil nil)
          r-outer (solve-node call-outer {:frontend frontend :env env2})
          outer-ty (get-type r-outer)]
      (is (tcon? outer-ty :int64) "f (f 42) 应为 int64"))))
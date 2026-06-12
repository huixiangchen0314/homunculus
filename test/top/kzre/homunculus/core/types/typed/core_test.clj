(ns top.kzre.homunculus.core.types.typed.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]  ;; MockFrontend, get-type, tcon?
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.ir2.model :as m])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(deftest infer-literal-test
  (let [frontend (->MockFrontend)]
    (testing "integer"
      (let [node (m/->LiteralNode 42 nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))
    (testing "float"
      (let [node (m/->LiteralNode 3.14 nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :float64))))
    (testing "string"
      (let [node (m/->LiteralNode "hello" nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :string))))
    (testing "bool"
      (let [node (m/->LiteralNode true nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :bool))))))

(deftest infer-variable-test
  (let [frontend (->MockFrontend)]
    (testing "bound variable"
      (let [node (m/->VariableNode "x" nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend :env {"x" (t/->TCon :int32)}})]
        (is (tcon? ty :int32))
        (is (tcon? (get-type result) :int32))))
    (testing "unbound variable throws"
      (let [node (m/->VariableNode "unknown" nil nil nil)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (typed/infer node {:frontend frontend :env {}})))))))

(deftest infer-call-test
  (let [frontend (->MockFrontend)
        builtins {'+ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))}]
    (testing "builtin call"
      (let [fn-node (m/->VariableNode "+" nil nil nil)
            arg1 (m/->LiteralNode 1 nil nil  nil)
            arg2 (m/->LiteralNode 2 nil nil  nil)
            call-node (m/->CallNode fn-node [arg1 arg2] nil nil nil)
            [ty result _] (typed/infer call-node {:frontend frontend :env builtins})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))))

(deftest infer-if-test
  (let [frontend (->MockFrontend)]
    (testing "if with both branches"
      (let [test-node (m/->LiteralNode true nil nil nil)
            then-node (m/->LiteralNode 1 nil nil nil)
            else-node (m/->LiteralNode 0 nil nil nil)
            if-node (m/->IfNode test-node then-node else-node nil nil nil)
            [ty result _] (typed/infer if-node {:frontend frontend})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))
    (testing "if without else"
      (let [test-node (m/->LiteralNode true nil nil nil)
            then-node (m/->LiteralNode "ok" nil nil nil)
            if-node (m/->IfNode test-node then-node nil nil nil nil)
            [ty result _] (typed/infer if-node {:frontend frontend})]
        (is (tcon? ty :string))))
    (testing "if test not bool throws"
      (is (thrown? clojure.lang.ExceptionInfo
                   (let [test-node (m/->LiteralNode 42 nil nil nil)
                         then-node (m/->LiteralNode 1 nil nil nil)
                         if-node (m/->IfNode test-node then-node nil nil nil nil)]
                     (typed/infer if-node {:frontend frontend})))))))

(deftest infer-block-test
  (let [frontend (->MockFrontend)
        exprs [(m/->LiteralNode 1 nil nil nil)
               (m/->LiteralNode "hello" nil nil nil)]
        block-node (m/->BlockNode exprs nil nil nil)
        [ty result _] (typed/infer block-node {:frontend frontend})]
    (is (tcon? ty :string))))

(deftest infer-let-test
  (let [frontend (->MockFrontend)]
    (testing "let binding"
      (let [val-node (m/->LiteralNode 10 nil nil nil)
            var-node (m/->VariableNode "x" nil nil nil)
            body-node (m/->VariableNode "x" nil nil nil)
            let-node (m/->LetNode [[var-node val-node]] body-node nil nil  nil)
            [ty result _] (typed/infer let-node {:frontend frontend})]
        (is (tcon? ty :int64))))
    (testing "let with annotation"
      (let [var-node (m/->VariableNode "x" {:tag "int64"} nil nil)
            val-node (m/->LiteralNode 10 nil nil nil)
            body-node (m/->VariableNode "x" nil nil nil)
            let-node (m/->LetNode [[var-node val-node]] body-node nil nil nil)
            [ty result _] (typed/infer let-node {:frontend frontend})]
        (is (tcon? ty :int64))))))

(deftest infer-lambda-test
  (let [frontend (->MockFrontend)]
    (testing "lambda type"
      (let [param-node (m/->VariableNode "x" nil nil  nil)
            body-node (m/->VariableNode "x" nil nil  nil)
            lambda-node (m/->LambdaNode [param-node] body-node [] nil nil nil  nil)
            [ty result _] (typed/infer lambda-node {:frontend frontend})]
        (is (tfun? ty))
        (is (tvar? (:arg ty)))
        (is (= (:arg ty) (:ret ty)))))
    (testing "lambda application"
      (let [param-node (m/->VariableNode "x" nil nil  nil)
            body-node (m/->VariableNode "x" nil nil  nil)
            lambda-node (m/->LambdaNode [param-node] body-node [] nil nil nil  nil)
            arg-node (m/->LiteralNode 10 nil nil  nil)
            call-node (m/->CallNode lambda-node [arg-node] nil nil  nil)
            [ty result _] (typed/infer call-node {:frontend frontend})]
        (is (tcon? ty :int64))))))

(deftest infer-loop-test
  (let [frontend (->MockFrontend)
        var-node (m/->VariableNode "x" nil nil nil)
        init-node (m/->LiteralNode 0 nil nil nil)
        test-true (m/->LiteralNode true nil nil nil)
        recur-node (m/->RecurNode [(m/->LiteralNode 1 nil nil nil)] nil nil nil)
        else-node (m/->VariableNode "x" nil nil nil)
        if-node (m/->IfNode test-true recur-node else-node nil nil  nil)
        loop-node (m/->LoopNode [[var-node init-node]] if-node nil nil nil)
        [ty result _] (typed/infer loop-node {:frontend frontend})]
    (is (tcon? ty :int64))))

(deftest infer-define-test
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 100 nil nil nil)
        define-node (m/->DefineNode 'y val-node nil nil nil nil)
        [ty result _] (typed/infer define-node {:frontend frontend})]
    (is (tcon? ty :int64))
    (is (= 'y (:name result)))))
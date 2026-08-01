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
      (let [node (m/->Literal 42 nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))
    (testing "float"
      (let [node (m/->Literal 3.14 nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :float64))))
    (testing "string"
      (let [node (m/->Literal "hello" nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :string))))
    (testing "bool"
      (let [node (m/->Literal true nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend})]
        (is (tcon? ty :bool))))))

(deftest infer-variable-test
  (let [frontend (->MockFrontend)]
    (testing "bound variable"
      (let [node (m/->Variable "x" nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend :env {"x" (t/->TCon :int32)}})]
        (is (tcon? ty :int32))
        (is (tcon? (get-type result) :int32))))
    (testing "unbound variable generates fresh TVar"
      (let [node (m/->Variable "unknown" nil nil nil)
            [ty result _] (typed/infer node {:frontend frontend :env {}})]
        (is (tvar? ty))
        (is (tvar? (get-type result)))))))

(deftest infer-call-test
  (let [frontend (->MockFrontend)
        builtins {'+ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))}]
    (testing "builtin call"
      (let [fn-node (m/->Variable "+" nil nil nil)
            arg1 (m/->Literal 1 nil nil  nil)
            arg2 (m/->Literal 2 nil nil  nil)
            call-node (m/->Call fn-node [arg1 arg2] nil nil nil)
            [ty result _] (typed/infer call-node {:frontend frontend :env builtins})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))))

(deftest infer-if-test
  (let [frontend (->MockFrontend)]
    (testing "if with both branches"
      (let [test-node (m/->Literal true nil nil nil)
            then-node (m/->Literal 1 nil nil nil)
            else-node (m/->Literal 0 nil nil nil)
            if-node (m/->If test-node then-node else-node nil nil nil)
            [ty result _] (typed/infer if-node {:frontend frontend})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))
    (testing "if without else"
      (let [test-node (m/->Literal true nil nil nil)
            then-node (m/->Literal "ok" nil nil nil)
            if-node (m/->If test-node then-node nil nil nil nil)
            [ty result _] (typed/infer if-node {:frontend frontend})]
        (is (tcon? ty :string))))
    (testing "if test not bool throws"
      (is (thrown? clojure.lang.ExceptionInfo
                   (let [test-node (m/->Literal 42 nil nil nil)
                         then-node (m/->Literal 1 nil nil nil)
                         if-node (m/->If test-node then-node nil nil nil nil)]
                     (typed/infer if-node {:frontend frontend})))))))

(deftest infer-block-test
  (let [frontend (->MockFrontend)
        exprs [(m/->Literal 1 nil nil nil)
               (m/->Literal "hello" nil nil nil)]
        block-node (m/->Block exprs nil nil nil)
        [ty result _] (typed/infer block-node {:frontend frontend})]
    (is (tcon? ty :string))))

(deftest infer-let-test
  (let [frontend (->MockFrontend)]
    (testing "let binding"
      (let [val-node (m/->Literal 10 nil nil nil)
            var-node (m/->Variable "x" nil nil nil)
            body-node (m/->Variable "x" nil nil nil)
            let-node (m/->Let [[var-node val-node]] body-node nil nil  nil)
            [ty result _] (typed/infer let-node {:frontend frontend})]
        (is (tcon? ty :int64))))
    (testing "let with annotation"
      (let [var-node (m/->Variable "x" {:tag "int64"} nil nil)
            val-node (m/->Literal 10 nil nil nil)
            body-node (m/->Variable "x" nil nil nil)
            let-node (m/->Let [[var-node val-node]] body-node nil nil nil)
            [ty result _] (typed/infer let-node {:frontend frontend})]
        (is (tcon? ty :int64))))))

(deftest infer-lambda-test
  (let [frontend (->MockFrontend)]
    (testing "lambda type"
      (let [param-node (m/->Variable "x" nil nil  nil)
            body-node (m/->Variable "x" nil nil  nil)
            lambda-node (m/->Lambda [param-node] body-node [] nil nil nil  nil)
            [ty result _] (typed/infer lambda-node {:frontend frontend})]
        (is (tfun? ty))
        (is (tvar? (:arg ty)))
        (is (= (:arg ty) (:ret ty)))))
    (testing "lambda application"
      (let [param-node (m/->Variable "x" nil nil  nil)
            body-node (m/->Variable "x" nil nil  nil)
            lambda-node (m/->Lambda [param-node] body-node [] nil nil nil  nil)
            arg-node (m/->Literal 10 nil nil  nil)
            call-node (m/->Call lambda-node [arg-node] nil nil  nil)
            [ty result _] (typed/infer call-node {:frontend frontend})]
        (is (tcon? ty :int64))))))

(deftest infer-loop-test
  (let [frontend (->MockFrontend)
        var-node (m/->Variable "x" nil nil nil)
        init-node (m/->Literal 0 nil nil nil)
        test-true (m/->Literal true nil nil nil)
        recur-node (m/->Recur [(m/->Literal 1 nil nil nil)] nil nil nil)
        else-node (m/->Variable "x" nil nil nil)
        if-node (m/->If test-true recur-node else-node nil nil  nil)
        loop-node (m/->Loop [[var-node init-node]] if-node nil nil nil)
        [ty result _] (typed/infer loop-node {:frontend frontend})]
    (is (tcon? ty :int64))))

(deftest infer-define-test
  (let [frontend (->MockFrontend)
        val-node (m/->Literal 100 nil nil nil)
        define-node (m/->Define 'y val-node nil nil nil nil)
        [ty result _] (typed/infer define-node {:frontend frontend})]
    (is (tcon? ty :int64))
    (is (= 'y (:name result)))))
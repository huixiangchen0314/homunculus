(ns top.kzre.homunculus.core.types.typed.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

;; 修正后的 MockFrontend，meta->type 可读取 :attrs 中的标签
(defrecord MockFrontend []
  tp/IFrontendInfo
  (frontend-types [_] [:int64 :float64 :bool :string :keyword :nil])
  (literal->type [_ val]
    (cond
      (instance? java.lang.Long val)    (t/->TCon :int64)
      (instance? java.lang.Double val)  (t/->TCon :float64)
      (instance? java.lang.Boolean val) (t/->TCon :bool)
      (instance? java.lang.String val)  (t/->TCon :string)
      (keyword? val)                    (t/->TCon :keyword)
      (nil? val)                        (t/->TCon :nil)
      :else (throw (ex-info "Unknown literal" {:val val}))))
  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]  ;; 同时检查 attrs
      (if (keyword? tag)
        (t/->TCon tag)
        (t/->TCon (keyword (name tag))))))
  (infer-collection-type [_ form] nil)
  (collection-type-ctor [_ kind element-type shape] nil))

(defn- get-type [node] (-> node ir2p/attrs :type))
(defn- tcon? [ty name] (and (instance? TCon ty) (= name (:name ty))))
(defn- tfun? [ty] (instance? TFun ty))
(defn- tvar? [ty] (instance? TVar ty))

(deftest infer-literal-test
  (let [frontend (->MockFrontend)]
    (testing "integer"
      (let [node (m/->LiteralNode 42 nil nil [] nil)
            [_ result] (infer/infer node {:frontend frontend})]
        (is (tcon? (get-type result) :int64))))
    (testing "float"
      (let [node (m/->LiteralNode 3.14 nil nil [] nil)
            [_ result] (infer/infer node {:frontend frontend})]
        (is (tcon? (get-type result) :float64))))
    (testing "string"
      (let [node (m/->LiteralNode "hello" nil nil [] nil)
            [_ result] (infer/infer node {:frontend frontend})]
        (is (tcon? (get-type result) :string))))
    (testing "bool"
      (let [node (m/->LiteralNode true nil nil [] nil)
            [_ result] (infer/infer node {:frontend frontend})]
        (is (tcon? (get-type result) :bool))))))

(deftest infer-variable-test
  (let [frontend (->MockFrontend)
        env {'x (t/->TCon :int32)}]
    (testing "bound variable"
      (let [node (m/->VariableNode "x" nil nil [] nil)
            [_ result] (infer/infer node {:frontend frontend :env env})]
        (is (tcon? (get-type result) :int32))))
    (testing "unbound variable throws"
      (let [node (m/->VariableNode "unknown" nil nil [] nil)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (infer/infer node {:frontend frontend :env {}})))))))

(deftest infer-call-test
  (let [frontend (->MockFrontend)
        builtins {'+ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))}]
    (testing "builtin call"
      (let [fn-node (m/->VariableNode "+" nil nil [] nil)
            arg1 (m/->LiteralNode 1 nil nil [] nil)
            arg2 (m/->LiteralNode 2 nil nil [] nil)
            call-node (m/->CallNode fn-node [arg1 arg2] nil nil [] nil)
            [_ result] (infer/infer call-node {:frontend frontend :env builtins})]
        (is (tcon? (get-type result) :int64))))))

(deftest infer-if-test
  (let [frontend (->MockFrontend)]
    (testing "if with both branches"
      (let [test-node (m/->LiteralNode true nil nil [] nil)
            then-node (m/->LiteralNode 1 nil nil [] nil)
            else-node (m/->LiteralNode 0 nil nil [] nil)
            if-node (m/->IfNode test-node then-node else-node nil nil [] nil)
            [_ result] (infer/infer if-node {:frontend frontend})]
        (is (tcon? (get-type result) :int64))))
    (testing "if without else"
      (let [test-node (m/->LiteralNode true nil nil [] nil)
            then-node (m/->LiteralNode "ok" nil nil [] nil)
            if-node (m/->IfNode test-node then-node nil nil nil [] nil)
            [_ result] (infer/infer if-node {:frontend frontend})]
        (is (tcon? (get-type result) :string))))
    (testing "if test not bool throws"
      (is (thrown? clojure.lang.ExceptionInfo
                   (let [test-node (m/->LiteralNode 42 nil nil [] nil)
                         then-node (m/->LiteralNode 1 nil nil [] nil)
                         if-node (m/->IfNode test-node then-node nil nil nil [] nil)]
                     (infer/infer if-node {:frontend frontend})))))))

(deftest infer-block-test
  (let [frontend (->MockFrontend)
        exprs [(m/->LiteralNode 1 nil nil [] nil)
               (m/->LiteralNode "hello" nil nil [] nil)]
        block-node (m/->BlockNode exprs nil nil [] nil)
        [_ result] (infer/infer block-node {:frontend frontend})]
    (is (tcon? (get-type result) :string))))

(deftest infer-let-test
  (let [frontend (->MockFrontend)]
    (testing "let binding"
      (let [val-node (m/->LiteralNode 10 nil nil [] nil)
            var-node (m/->VariableNode "x" nil nil [] nil)
            body-node (m/->VariableNode "x" nil nil [] nil)
            let-node (m/->LetNode [[var-node val-node]] body-node nil nil [] nil)
            [_ result] (infer/infer let-node {:frontend frontend})]
        (is (tcon? (get-type result) :int64))))
    (testing "let with annotation"
      (let [var-node (m/->VariableNode "x" {:tag "int32"} nil [] nil)
            val-node (m/->LiteralNode 10 nil nil [] nil)
            body-node (m/->VariableNode "x" nil nil [] nil)
            let-node (m/->LetNode [[var-node val-node]] body-node nil nil [] nil)
            [_ result] (infer/infer let-node {:frontend frontend})]
        (is (tcon? (get-type result) :int32))))))

(deftest infer-lambda-test
  (let [frontend (->MockFrontend)]
    (testing "lambda type"
      (let [param-node (m/->VariableNode "x" nil nil [] nil)
            body-node (m/->VariableNode "x" nil nil [] nil)
            lambda-node (m/->LambdaNode [param-node] body-node [] nil nil nil [] nil)
            [_ result] (infer/infer lambda-node {:frontend frontend})]
        (is (tfun? (get-type result)))
        (is (tvar? (:arg (get-type result))))
        (is (= (:arg (get-type result)) (:ret (get-type result))))))
    (testing "lambda application"
      (let [param-node (m/->VariableNode "x" nil nil [] nil)
            body-node (m/->VariableNode "x" nil nil [] nil)
            lambda-node (m/->LambdaNode [param-node] body-node [] nil nil nil [] nil)
            arg-node (m/->LiteralNode 10 nil nil [] nil)
            call-node (m/->CallNode lambda-node [arg-node] nil nil [] nil)
            [_ result] (infer/infer call-node {:frontend frontend})]
        (is (tcon? (get-type result) :int64))))))

(deftest infer-loop-test
  (let [frontend (->MockFrontend)
        var-node (m/->VariableNode "x" nil nil [] nil)
        init-node (m/->LiteralNode 0 nil nil [] nil)
        test-true (m/->LiteralNode true nil nil [] nil)
        recur-node (m/->RecurNode [(m/->LiteralNode 1 nil nil [] nil)] nil nil [] nil)
        else-node (m/->VariableNode "x" nil nil [] nil)
        if-node (m/->IfNode test-true recur-node else-node nil nil [] nil)
        loop-node (m/->LoopNode [[var-node init-node]] if-node nil nil [] nil)
        [_ result] (infer/infer loop-node {:frontend frontend})]
    (is (tcon? (get-type result) :int64))))

(deftest infer-define-test
  (let [frontend (->MockFrontend)
        val-node (m/->LiteralNode 100 nil nil [] nil)
        define-node (m/->DefineNode 'y val-node nil nil nil [] nil)
        [_ result] (infer/infer define-node {:frontend frontend})]
    (is (tcon? (get-type result) :int64))
    (is (= 'y (:name result)))))
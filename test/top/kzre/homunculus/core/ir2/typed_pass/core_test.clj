(ns top.kzre.homunculus.core.ir2.typed-pass.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.typed-pass.core :as tp]
            [top.kzre.homunculus.core.ir2.typed-pass.methods]   ;; 注册多方法
            [top.kzre.homunculus.core.ir2.typed-pass.types :as t]
            [top.kzre.homunculus.core.ir2.typed-pass.env :as e])
  (:import (top.kzre.homunculus.core.ir2.typed_pass.types TVar TCon TFun)))

;; 辅助：检查类型是否为指定名称的 TCon
(defn tcon? [ty name]
  (and (instance? TCon ty) (= name (:name ty))))

;; 辅助：创建一个内置环境（可以在这里定义常用函数的类型）
(def builtins
  {'+ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   'inc (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
   'println (t/->TFun (t/->TCon :string) (t/->TCon :nil))})

;; ── 字面量测试 ──────────────────────────
(deftest infer-literal-test
  (testing "integer literal"
    (let [[ty node] (tp/infer {:kind :literal :val 42 :attrs {}} {})]
      (is (tcon? ty :int64))
      (is (= ty (get-in node [:attrs :type])))))
  (testing "float literal"
    (let [[ty node] (tp/infer {:kind :literal :val 3.14 :attrs {}} {})]
      (is (tcon? ty :float64))))
  (testing "string literal"
    (let [[ty node] (tp/infer {:kind :literal :val "hello" :attrs {}} {})]
      (is (tcon? ty :string))))
  (testing "boolean literal"
    (let [[ty node] (tp/infer {:kind :literal :val true :attrs {}} {})]
      (is (tcon? ty :bool))))
  (testing "nil literal"
    (let [[ty node] (tp/infer {:kind :literal :val nil :attrs {}} {})]
      (is (tcon? ty :nil)))))

;; ── 变量测试 ──────────────────────────
(deftest infer-variable-test
  (testing "unbound variable throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (tp/infer {:kind :variable :name 'x :attrs {}} {}))))
  (testing "variable from environment"
    (let [env (e/extend-env {} 'x (t/->TCon :int32))
          [ty node] (tp/infer {:kind :variable :name 'x :attrs {}} env)]
      (is (tcon? ty :int32))
      (is (= ty (get-in node [:attrs :type]))))))

;; ── 调用测试 ──────────────────────────
(deftest infer-call-test
  (testing "call to builtin +"
    (let [env builtins
          node {:kind :call
                :fn {:kind :variable :name '+}
                :args [{:kind :literal :val 1 :attrs {}}
                       {:kind :literal :val 2 :attrs {}}]
                :attrs {}}
          [ty result] (tp/infer node env)]
      (is (tcon? ty :int64))
      (is (= ty (get-in result [:attrs :type])))))
  (testing "call to unbound function throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (tp/infer {:kind :call
                            :fn {:kind :variable :name 'unknown}
                            :args []}
                           {})))))

;; ── if 测试 ──────────────────────────
(deftest infer-if-test
  (testing "if with both branches"
    (let [node {:kind :if
                :test {:kind :literal :val true :attrs {}}
                :then {:kind :literal :val 1 :attrs {}}
                :else {:kind :literal :val 0 :attrs {}}
                :attrs {}}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :int64))
      (is (= ty (get-in result [:attrs :type])))))
  (testing "if without else"
    (let [node {:kind :if
                :test {:kind :literal :val true :attrs {}}
                :then {:kind :literal :val "ok" :attrs {}}
                :attrs {}}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :string))))
  (testing "if test not bool throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (tp/infer {:kind :if
                            :test {:kind :literal :val 42 :attrs {}}
                            :then {:kind :literal :val 1 :attrs {}}
                            :attrs {}}
                           {})))))

;; ── block 测试 ──────────────────────────
(deftest infer-block-test
  (testing "block returns last expression type"
    (let [node {:kind :block
                :exprs [{:kind :literal :val 1 :attrs {}}
                        {:kind :literal :val "hello" :attrs {}}]
                :attrs {}}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :string))))
  (testing "empty block returns nil"
    (let [node {:kind :block :exprs [] :attrs {}}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :nil)))))

;; ── let 测试 ──────────────────────────
(deftest infer-let-test
  (testing "let binding with single body"
    (let [node {:kind :let
                :bindings [[{:kind :variable :name 'x :attrs {}}
                            {:kind :literal :val 10 :attrs {}}]]
                :body {:kind :variable :name 'x :attrs {}}
                :attrs {}}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :int64))))
  (testing "let binding with user type annotation"
    (let [node {:kind :let
                :bindings [[{:kind :variable :name 'x :attrs {:tag "int32"}}
                            {:kind :literal :val 10 :attrs {}}]]
                :body {:kind :variable :name 'x :attrs {}}
                :attrs {}}
          [ty result] (tp/infer node {})]
      ;; 由于用户标注了 :int32，绑定的类型应该是 :int32
      (is (tcon? ty :int32)))))

;; ── lambda 测试 ──────────────────────────
(deftest infer-lambda-test
  (testing "lambda type inference"
    (let [node {:kind :lambda
                :params [{:kind :variable :name 'x :attrs {}}]
                :body {:kind :variable :name 'x :attrs {}}
                :captures []
                :attrs {}}
          [ty result] (tp/infer node {})]
      (is (instance? TFun ty))
      (is (instance? TVar (:arg ty)))   ;; 参数类型是类型变量
      (is (= (:arg ty) (:ret ty)))))      ;; 返回类型与参数类型相同（未统一前是同一个 TVar）
  (testing "lambda application"
    (let [;; (fn [x] x) 10
          node {:kind :call
                :fn {:kind :lambda
                     :params [{:kind :variable :name 'x :attrs {}}]
                     :body {:kind :variable :name 'x :attrs {}}
                     :captures []}
                :args [{:kind :literal :val 10 :attrs {}}]}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :int64)))))

;; ── loop / recur 测试 ────────────────────
(deftest infer-loop-test
  (testing "simple loop with recur"
    (let [node {:kind :loop
                :bindings [[{:kind :variable :name 'x :attrs {}}
                            {:kind :literal :val 0 :attrs {}}]]
                :body {:kind :if
                       :test {:kind :literal :val true :attrs {}}   ;; 永远真，简化
                       :then {:kind :recur :args [{:kind :literal :val 1 :attrs {}}] :attrs {}}
                       :else {:kind :variable :name 'x :attrs {}}
                       :attrs {}}
                :attrs {}}
          [ty result] (tp/infer node {})]
      ;; 由于 then 分支是 recur，else 分支返回 x (int64)，整体类型应为 int64
      (is (tcon? ty :int64))))
  (testing "recur outside loop throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (tp/infer {:kind :recur :args [{:kind :literal :val 1}] :attrs {}} {}))))
  (testing "recur arg count mismatch throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (tp/infer {:kind :loop
                            :bindings [[{:kind :variable :name 'x}
                                        {:kind :literal :val 0}]]
                            :body {:kind :recur :args [{:kind :literal :val 1}
                                                       {:kind :literal :val 2}]}} ;; 多了一个参数
                           {})))))

;; ── define 测试 ──────────────────────────
(deftest infer-define-test
  (testing "define adds to environment"
    (let [node {:kind :define :name 'y :val {:kind :literal :val 100 :attrs {}} :attrs {}}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :int64))
      (is (= ty (get-in result [:attrs :type])))))
  (testing "define and then reference"
    (let [program [{:kind :define :name 'a :val {:kind :literal :val 3.0 :attrs {}} :attrs {}}
                   {:kind :variable :name 'a :attrs {}}]
          ;; 使用 type-check 连续处理
          results (tp/type-check program)]
      (is (= 2 (count results)))
      (let [first-def (first results)
            ref (second results)]
        (is (tcon? (get-in first-def [:attrs :type]) :float64))
        (is (tcon? (get-in ref [:attrs :type]) :float64))))))

;; ── 用户元数据标注测试 ───────────────────
(deftest meta-annotation-test
  (testing "variable with meta tag overrides inference"
    (let [node {:kind :variable :name 'x :attrs {:tag "float32"} :meta {:tag "float32"}}
          ;; 即使环境中有绑定为其他类型，meta 优先
          env {'x (t/->TCon :int32)}
          [ty result] (tp/infer node env)]
      (is (tcon? ty :float32))))
  (testing "literal with meta tag"
    (let [node {:kind :literal :val 42 :meta {:tag "int32"} :attrs {}}
          [ty result] (tp/infer node {})]
      (is (tcon? ty :int32)))))
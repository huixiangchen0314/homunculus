(ns top.kzre.homunculus.core.types.subst-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.subst :as sut]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.protocol :as p]))

;; ── 辅助构造函数 ──
(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- lam [params body] (m/->LambdaNode params body [] nil nil nil nil))
(defn- call [fn-node & args] (m/->CallNode fn-node (vec args) nil nil nil))
(defn- let-node [bindings body] (m/->LetNode (vec bindings) body nil nil nil))
(defn- define [name val] (m/->DefineNode name val nil nil nil nil))
(defn- block [& exprs] (m/->BlockNode (vec exprs) nil nil nil))
(defn- assign [var val] (m/->AssignNode var val nil nil nil))
(defn- while-node [test body] (m/->WhileNode test body nil nil nil))
(defn- if-node [test then else] (m/->IfNode test then else nil nil nil))
(defn- try-node [body catches finally] (m/->TryNode body catches finally nil nil nil))
(defn- catch-node [class sym body]
  (m/->CatchNode (vref class) (vref sym) body nil nil nil))   ;; class, sym 用 vref
(defn- throw-node [expr] (m/->ThrowNode expr nil nil nil))

;; 虚拟配置（用于 lift-lambda 测试）
(defrecord DummyConfig []
  p/IInlineLiftConfig
  (should-inline? [_ _ _] true)
  (should-lift? [_ _] true)
  (max-inline-size [_] 10)
  (lift-name-gen [_ lambda-node] 'lifted-fn))

;; ── inline-call 测试 ─────────────────────
(deftest inline-call-test
  (testing "identity function"
    (let [lam (lam [(vref "x")] (vref "x"))
          call-node (call lam (lit 42))
          result (sut/inline-call call-node lam nil)]
      (is (and (satisfies? ir2p/INode result)
               (= (ir2p/kind result) :literal)
               (= (:val result) 42)))))
  (testing "addition"
    (let [lam (lam [(vref "a") (vref "b")] (call (vref "+") (vref "a") (vref "b")))
          call-node (call lam (lit 3) (lit 4))
          result (sut/inline-call call-node lam nil)
          kind (ir2p/kind result)]
      ;; 替换后应为 (+ 3 4) 调用
      (is (= kind :call))
      (is (= (ir2p/kind (:fn result)) :variable))
      (is (= (:name (:fn result)) "+"))
      (is (= 2 (count (:args result))))
      (is (every? #(= (ir2p/kind %) :literal) (:args result))))))

;; ── lift-lambda 测试 ─────────────────────
(deftest lift-lambda-test
  (testing "no free vars"
    (let [lam (lam [(vref "x")] (vref "x"))
          {:keys [define ref]} (sut/lift-lambda lam #{} (->DummyConfig))]
      (is (= (ir2p/kind define) :define))
      (is (= (:name define) 'lifted-fn))
      (is (= (ir2p/kind ref) :variable))
      (is (= (:name ref) "lifted-fn"))))
  (testing "with free vars"
    (let [lam (lam [(vref "x")] (call (vref "f") (vref "x") (vref "y")))
          ;; y 是自由变量
          {:keys [define ref]} (sut/lift-lambda lam #{"y"} (->DummyConfig))]
      (is (= (ir2p/kind define) :define))
      (let [lifted-lam (:val define)]
        (is (= (ir2p/kind lifted-lam) :lambda))
        ;; 参数应增加一个（y）
        (is (= 2 (count (:params lifted-lam))))
        (is (= "y" (:name (last (:params lifted-lam)))))
        ;; 函数体应为 let [y y] 包裹的原体
        (is (= (ir2p/kind (:body lifted-lam)) :let))))))

;; ── inline-expr 测试 ─────────────────────
(deftest inline-expr-test
  (testing "replace variable in call"
    (let [node (call (vref "f") (vref "x"))
          result (sut/inline-expr node "x" (lit 99))]
      (is (= (ir2p/kind result) :call))
      (let [args (:args result)]
        (is (= 1 (count args)))
        (is (and (= (ir2p/kind (first args)) :literal)
                 (= (:val (first args)) 99))))))
  (testing "replace in let body"
    (let [node (let-node [[(vref "y") (lit 1)]]
                         (vref "x"))
          result (sut/inline-expr node "x" (lit 100))]
      (is (= (ir2p/kind result) :let))
      (let [body (:body result)]
        (is (and (= (ir2p/kind body) :literal)
                 (= (:val body) 100))))))
  (testing "replace in block"
    (let [node (block (vref "a") (vref "b"))
          result (sut/inline-expr node "a" (lit 1))]
      (is (= (ir2p/kind result) :block))
      (let [first-expr (first (:exprs result))]
        (is (and (= (ir2p/kind first-expr) :literal)
                 (= (:val first-expr) 1))))))
  (testing "replace in while"
    (let [node (while-node (vref "cond") (vref "body"))
          result (sut/inline-expr node "cond" (lit true))]
      (is (= (ir2p/kind result) :while))
      (is (and (= (ir2p/kind (:test result)) :literal)
               (= (:val (:test result)) true)))))
  (testing "replace in assign"
    (let [node (assign (vref "x") (vref "y"))
          result (sut/inline-expr node "x" (vref "z"))]
      (is (= (ir2p/kind result) :assign))
      (is (= (:name (:var result)) "z")))
    (let [node (assign (vref "x") (vref "y"))
          result (sut/inline-expr node "y" (lit 5))]
      (is (= (ir2p/kind result) :assign))
      (is (and (= (ir2p/kind (:val result)) :literal)
               (= (:val (:val result)) 5)))))
  (testing "replace in try/catch/throw"
    (let [node (try-node [(vref "a")] [(catch-node "Exception" "e" [(vref "e")])] [(vref "finally")])
          result (sut/inline-expr node "a" (lit 42))]
      (is (= (ir2p/kind result) :try))
      (let [first-body (first (:body result))]
        (is (and (= (ir2p/kind first-body) :literal)
                 (= (:val first-body) 42))))))
  (testing "no replacement for unrelated variable"
    (let [node (call (vref "f") (vref "y"))
          result (sut/inline-expr node "x" (lit 1))]
      ;; 节点不应改变
      (is (= (ir2p/kind result) :call))
      (is (= (:name (first (:args result))) "y")))))

;; ── 边缘情况 ──────────────────────────
(deftest edge-cases
  (testing "inline-expr on literal"
    (let [node (lit 42)
          result (sut/inline-expr node "x" (lit 1))]
      (is (and (= (ir2p/kind result) :literal)
               (= (:val result) 42)))))
  (testing "inline-expr on variable no match"
    (let [node (vref "y")
          result (sut/inline-expr node "x" (lit 1))]
      (is (and (= (ir2p/kind result) :variable)
               (= (:name result) "y")))))
  (testing "inline-expr on variable match"
    (let [node (vref "x")
          result (sut/inline-expr node "x" (lit 99))]
      (is (and (= (ir2p/kind result) :literal)
               (= (:val result) 99)))))
  (testing "inline-call with multi-arg lambda"
    (let [lam (lam [(vref "a") (vref "b")] (call (vref "+") (vref "a") (vref "b")))
          call-node (call lam (lit 7) (lit 8))
          result (sut/inline-call call-node lam nil)]
      (is (= (ir2p/kind result) :call))
      (is (= (count (:args result)) 2)))))
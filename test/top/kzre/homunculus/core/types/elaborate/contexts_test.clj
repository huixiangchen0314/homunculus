(ns top.kzre.homunculus.core.types.elaborate.contexts-test
  "elaborate pass 上下文收集的独立测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

;; 辅助构造函数
(defn- vref [name] (m/->VariableNode name nil nil [] nil))
(defn- lit [val] (m/->LiteralNode val nil nil [] nil))
(defn- lam [params body] (m/->LambdaNode params body [] nil nil nil (vec (concat params [body])) nil))
(defn- call [fn-node & args] (m/->CallNode fn-node (vec args) nil nil (vec (cons fn-node args)) nil))
(defn- define [name val] (m/->DefineNode name val nil nil nil (if val [name val] [name]) nil))
(defn- let-node [bindings body] (m/->LetNode (vec bindings) body nil nil (vec (concat (mapcat (fn [[v val]] [v val]) bindings) [body])) nil))
(defn- block [& exprs] (m/->BlockNode (vec exprs) nil nil (vec exprs) nil))
(defn- assign [var val] (m/->AssignNode var val nil nil [var val] nil))

(deftest collect-fn-role-test
  (testing "闭包在直接调用位置，角色应为 :fn"
    (let [lam (lam [(vref "x")] (vref "x"))
          call-node (call lam (lit 42))
          ctxs (elaborate/lambda-contexts [call-node])]
      (is (= 1 (count ctxs)))
      (let [ctx (first ctxs)]
        (is (= (ir2p/kind (:lambda ctx)) :lambda))
        (is (= (:role ctx) :fn))
        (is (identical? (:parent ctx) call-node))))))

(deftest collect-let-val-role-test
  (testing "闭包在 let 绑定值位置，角色应为 :let-val"
    (let [lam (lam [(vref "x")] (vref "x"))
          var (vref "f")
          let-node (let-node [[var lam]] (call var (lit 1)))
          ctxs (elaborate/lambda-contexts [let-node])]
      (is (= 1 (count ctxs)))
      (let [ctx (first ctxs)]
        (is (= (:role ctx) :let-val))
        (is (identical? (:parent ctx) let-node))))))

(deftest collect-args-to-known-fn-role-test
  (testing "闭包作为实参传递给已知顶层函数，角色应为 :args-to-known-fn 并携带目标函数名"
    (let [g-lambda (lam [(vref "f") (vref "x")] (call (vref "f") (vref "x")))
          def-g (define 'g g-lambda)
          closure (lam [(vref "a")] (vref "a"))
          call-node (call (vref "g") closure (lit 10))
          roots [def-g call-node]
          ctxs (elaborate/lambda-contexts roots)]
      ;; 注意：g-lambda 中的 lambda 被 define 内部，不应收集；closure 会被收集
      (is (= 1 (count ctxs)))
      (let [ctx (first ctxs)]
        (is (= (:role ctx) :args-to-known-fn))
        (is (= 0 (:index ctx)))
        (is (= 'g (:target-fn-name ctx)))))))

(deftest collect-lambda-body-role-test
  (testing "闭包出现在另一个 lambda 的 body 中，角色应为 :lambda-body"
    (let [inner-lam (lam [(vref "x")] (vref "x"))
          outer-lam (lam [(vref "y")] inner-lam)
          ctxs (elaborate/lambda-contexts [outer-lam])]
      ;; 外层 lambda 也会被收集，总数为 2
      (is (= 2 (count ctxs)))
      ;; 找到内层 lambda 的上下文
      (let [inner-ctx (first (filter #(= (:lambda %) inner-lam) ctxs))]
        (is (some? inner-ctx))
        (is (= (:role inner-ctx) :lambda-body))
        (is (identical? (:parent inner-ctx) outer-lam))))))

(deftest collect-assign-val-role-test
  (testing "闭包赋值给 assign 节点，角色应为 :assign-val"
    (let [lam (lam [(vref "x")] (vref "x"))
          assign-node (assign (vref "y") lam)
          ctxs (elaborate/lambda-contexts [assign-node])]
      (is (= 1 (count ctxs)))
      (let [ctx (first ctxs)]
        (is (= (:role ctx) :assign-val))
        (is (identical? (:parent ctx) assign-node))))))

(deftest ignore-toplevel-define-lambda-test
  (testing "顶层 define 内部的 lambda 不应被收集"
    (let [lam (lam [(vref "x")] (vref "x"))
          def (define 'id lam)
          ctxs (elaborate/lambda-contexts [def])]
      (is (empty? ctxs)))))
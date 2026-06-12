(ns top.kzre.homunculus.core.types.elaborate.core-test
  "elaborate pass 基础单元测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.elaborate.protocol :as cfg]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

;; 辅助构造函数
(defn- vref [name] (m/->VariableNode name nil nil [] nil))
(defn- lit [val] (m/->LiteralNode val nil nil [] nil))
(defn- lam [params body] (m/->LambdaNode params body [] nil nil nil (vec (concat params [body])) nil))
(defn- call [fn-node & args] (m/->CallNode fn-node (vec args) nil nil (vec (cons fn-node args)) nil))
(defn- define [name val] (m/->DefineNode name val nil nil nil (if val [name val] [name]) nil))

(defrecord TestConfig []
  cfg/IElaborateConfig
  (max-iterations [_] 5)
  (strict-mode? [_] false)    ;; 设为 false 以避免未实现功能导致异常
  (allow-return-closure? [_] false)
  (on-unresolved [_ lambda] (println "Unresolved closure:" (:name lambda))))

;; 测试 1：没有闭包时，直接返回原 IR2
(deftest no-closures-test
  (let [config (->TestConfig)
        roots [(lit 42) (call (vref "+") (lit 1) (lit 2))]
        result (elaborate/elaborate roots config)]
    (is (= roots result))))

;; 测试 2：顶层 define 中的 lambda 应保留
(deftest toplevel-define-lambda-test
  (let [config (->TestConfig)
        id-lambda (lam [(vref "x")] (vref "x"))
        def (define 'id id-lambda)
        roots [def]
        result (elaborate/elaborate roots config)]
    (is (= roots result))))

;; 测试 3：自由变量分析
(deftest free-vars-test
  (let [lam (lam [(vref "x")] (call (vref "f") (vref "y")))]
    (is (= #{"f" "y"} (elaborate/free-vars lam)))))

;; 测试 4：let 绑定闭包的内联（基本端到端测试）
(deftest inline-let-lambda-test
  (let [config (->TestConfig)
        id-lambda (lam [(vref "x")] (vref "x"))
        f-var (vref "f")
        body (call f-var (lit 42))
        let-node (m/->LetNode [[f-var id-lambda]] body nil nil [] nil)
        roots [let-node]
        result (elaborate/elaborate roots config)]
    ;; 期望：let 绑定被内联，最终变为字面量 42
    (is (= 1 (count result)))
    (let [root (first result)]
      (is (and (satisfies? ir2p/INode root)
               (= (ir2p/kind root) :literal)
               (= (:val root) 42))))))

;; 测试 5：直接调用 lambda 的内联
(deftest direct-call-lambda-test
  (let [config (->TestConfig)
        lam (lam [(vref "x")] (vref "x"))
        call-node (call lam (lit 42))
        roots [call-node]
        result (elaborate/elaborate roots config)]
    (is (= 1 (count result)))
    (let [root (first result)]
      (is (and (satisfies? ir2p/INode root)
               (= (ir2p/kind root) :literal)
               (= (:val root) 42))))))
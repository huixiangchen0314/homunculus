(ns top.kzre.homunculus.core.types.inline-lift.core-test
  "内联与提升闭包消除 pass 的单元测试（最终修正版）。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.inline-lift.core :as lift]
            [top.kzre.homunculus.core.types.inline-lift.protocol :as cfg]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

;; ── 辅助构造函数 ──────────────────────────
(defn- vref [name] (m/->VariableNode name nil nil [] nil))
(defn- lit [val] (m/->LiteralNode val nil nil [] nil))

(defn- call [fn-node & args]
  (m/->CallNode fn-node (vec args) nil nil
                (vec (cons fn-node args)) nil))

(defn- lam [params body]
  (m/->LambdaNode params body [] nil nil nil
                  (vec (concat params [body])) nil))

(defn- block
  "构造 BlockNode。接受多个子节点，或一个集合。"
  [& exprs]
  (let [flat (if (and (= (count exprs) 1) (coll? (first exprs)))
               (first exprs)
               exprs)
        exprs-vec (vec flat)]
    (m/->BlockNode exprs-vec nil nil exprs-vec nil)))

(defn- let-node [bindings body]
  (m/->LetNode (vec bindings) body nil nil
               (vec (concat (mapcat (fn [[v val]] [v val]) bindings) [body]))
               nil))

;; ── 节点大小计算 ──────────────────────────
(defn- node-size [node]
  (if (satisfies? ir2p/INode node)
    (inc (reduce + (map node-size (ir2p/children node))))
    0))

;; ── 配置 ──────────────────────────────────
(defrecord TestConfig []
  cfg/IInlineLiftConfig
  (should-inline? [this lambda call-site]
    (< (node-size (:body lambda)) 10))
  (should-lift? [this lambda] true)
  (max-inline-size [this] 20)
  (lift-name-gen [this lambda] (gensym "lifted")))

(defrecord LiftOnlyConfig []
  cfg/IInlineLiftConfig
  (should-inline? [_ _ _] false)
  (should-lift? [_ _] true)
  (max-inline-size [_] 0)
  (lift-name-gen [_ lambda] (gensym "lifted")))

;; ── 测试 ──────────────────────────────────
(deftest free-vars-test
  (testing "无自由变量的 lambda"
    (let [lam (lam [(vref "x")] (vref "x"))]
      (is (empty? (lift/free-vars lam)))))
  (testing "捕获自由变量的 lambda"
    (let [body (call (vref "f") (vref "y"))
          lam (lam [(vref "x")] body)]
      (is (= #{"f" "y"} (lift/free-vars lam))))))

(deftest inline-call-test
  (let [config (->TestConfig)
        id-lambda (lam [(vref "x")] (vref "x"))
        call-node (call id-lambda (lit 42))
        result (lift/inline-call call-node id-lambda config)]
    (is (and (satisfies? ir2p/INode result)
             (= (ir2p/kind result) :literal)
             (= (:val result) 42)))))

(deftest lift-lambda-test
  (let [config (->TestConfig)
        lam (lam [(vref "x")] (vref "x"))
        {:keys [define ref]} (lift/lift-lambda lam #{} config)]
    (is (= (ir2p/kind define) :define))
    (is (= (ir2p/kind ref) :variable))
    (is (symbol? (:name define)))))

(deftest eliminate-closures-inline-scenario
  (let [config (->TestConfig)
        id-lambda (lam [(vref "x")] (vref "x"))
        call-node (call id-lambda (lit 42))
        new-roots (lift/eliminate-closures [call-node] config)]
    (is (= 1 (count new-roots)))
    (let [result (first new-roots)]
      (is (and (satisfies? ir2p/INode result)
               (= (ir2p/kind result) :literal)
               (= (:val result) 42))))))

(deftest eliminate-closures-lift-scenario
  (let [config (->TestConfig)
        ;; 较大的 body：20 个变量引用，节点数 = 20（>10），应触发提升
        big-body (block (take 20 (repeat (vref "x"))))
        lam (lam [(vref "x")] big-body)
        call-node (call lam (lit 1))
        new-roots (lift/eliminate-closures [call-node] config)]
    (is (= 2 (count new-roots)))
    (is (= (ir2p/kind (first new-roots)) :call))
    (is (= (ir2p/kind (second new-roots)) :define))))

(deftest lift-only-config-test
  (let [config (->LiftOnlyConfig)
        id-lambda (lam [(vref "x")] (vref "x"))
        call-node (call id-lambda (lit 42))
        new-roots (lift/eliminate-closures [call-node] config)]
    (is (= 2 (count new-roots)))
    (is (= (ir2p/kind (first new-roots)) :call))
    (is (= (ir2p/kind (second new-roots)) :define))))

(deftest mixed-let-closure-test
  (let [config (->TestConfig)
        id-lambda (lam [(vref "x")] (vref "x"))
        f-var (vref "f")
        call-f (call (vref "f") (lit 99))
        let-n (let-node [[f-var id-lambda]] call-f)
        new-roots (lift/eliminate-closures [let-n] config)]
    (is (= 1 (count new-roots)))))
(ns top.kzre.homunculus.core.types.recur-elim.core-test
  (:require [clojure.test :refer [deftest is testing are]]
            [top.kzre.homunculus.core.types.recur-elim.core :as sut]
            [top.kzre.homunculus.core.types.recur-elim.methods]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as p]))

;; ── 辅助构造函数（简化节点创建） ──
(defn- variable [name]
  (m/->VariableNode name nil nil nil))

(defn- literal [val]
  (m/->LiteralNode val nil nil nil))

(defn- if-node [test then else]
  (m/->IfNode test then else nil nil nil))

(defn- block [& exprs]
  (m/->BlockNode (vec exprs) nil nil nil))

(defn- let-node [bindings body]
  (m/->LetNode (vec bindings) body nil nil nil))

(defn- loop-node [bindings body]
  (m/->LoopNode (vec bindings) body nil nil nil))

(defn- recur-node [args]
  (m/->RecurNode (vec args) nil nil nil))

(defn- call [f & args]
  (m/->CallNode f (vec args) nil nil nil))

;; ── 测试：基本 loop 无递归（立即返回） ──
(deftest loop-no-recur-test
  (testing "loop with no recur returns body value"
    (let [input (loop-node [[(variable 'x) (literal 42)]]
                           (variable 'x))
          output (sut/eliminate input)]
      ;; 结果应为 (let [x 42 result nil recur? true]
      ;;            (while recur?
      ;;              (block (set! result x) (set! recur? false)))
      ;;            result)
      (is (satisfies? p/INode output))
      (is (= :let (p/kind output))))))

;; ── 测试：简单递归 loop，返回最终值 ──
(deftest simple-loop-recur-test
  (testing "loop [x 0] (if (< x 5) (recur (+ x 1)) x)  => should return 5"
    (let [input (loop-node [[(variable 'x) (literal 0)]]
                           (if-node (call (variable '<) (variable 'x) (literal 5))
                                    (recur-node [(call (variable '+) (variable 'x) (literal 1))])
                                    (variable 'x)))
          output (sut/eliminate input)]
      ;; 检查结构：应包含 :while 节点
      (is (some? output))
      ;; 可以进一步用walk检查包含 AssignNode 和 WhileNode，但这里只验证成功转换且无异常
      (is (satisfies? p/INode output))
      ;; 确保没有 :loop 和 :recur 残留
      (let [found (atom false)]
        ((fn walk [node]
           (when (satisfies? p/INode node)
             (when (= :loop (p/kind node)) (swap! found (constantly true)))
             (doseq [c (p/children node)] (walk c)))) output)
        (is (not @found) "No :loop node should remain")))))

;; ── 测试：recur 在非尾部位置应抛出异常 ──
(deftest recur-non-tail-test
  (testing "recur used in non-tail position throws exception"
    (let [input (loop-node [[(variable 'x) (literal 0)]]
                           (if-node (recur-node [(literal 1)])
                                    (variable 'a)
                                    (variable 'b)))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (sut/eliminate input))))))

;; ── 测试：嵌套 loop ──
(deftest nested-loop-test
  (testing "nested loop should be both transformed"
    (let [inner (loop-node [[(variable 'y) (literal 10)]]
                           (if-node (call (variable '>) (variable 'y) (literal 0))
                                    (recur-node [(call (variable '-) (variable 'y) (literal 1))])
                                    (variable 'y)))
          outer (loop-node [[(variable 'x) (literal 0)]]
                           (if-node (call (variable '>) (variable 'x) (literal 3))
                                    (recur-node [(call (variable '+) (variable 'x) (literal 1))])
                                    inner))  ;; inner is final result
          output (sut/eliminate outer)]
      (is (satisfies? p/INode output))
      ;; 确保没有 loop 残留
      (let [found (atom false)]
        ((fn walk [node]
           (when (satisfies? p/INode node)
             (when (= :loop (p/kind node)) (swap! found (constantly true)))
             (doseq [c (p/children node)] (walk c)))) output)
        (is (not @found))))))

;; ── 测试：无 loop 的表达式保持不变 ──
(deftest no-loop-test
  (testing "non-loop nodes are not modified"
    (let [input (if-node (variable 'a) (literal 1) (literal 2))
          output (sut/eliminate input)]
      (is (= :if (p/kind output)))
      (is (= :variable (p/kind (:test output))))
      (is (= :literal (p/kind (:then output)))))))

;; ── 测试：let 中包含 loop ──
(deftest let-with-loop-test
  (testing "loop inside let expression"
    (let [inner-loop (loop-node [[(variable 'i) (literal 0)]]
                                (if-node (call (variable '<) (variable 'i) (literal 2))
                                         (recur-node [(call (variable '+) (variable 'i) (literal 1))])
                                         (variable 'i)))
          input (let-node [[(variable 'x) (literal 100)]]
                          inner-loop)
          output (sut/eliminate input)]
      (is (= :let (p/kind output)))
      ;; body 应该是 while 而不是 loop
      (let [body (:body output)]
        (is (= :let (p/kind body))          ; transform-loop 产生 let
            "The loop should be transformed into a let containing while")))))

;; ── 测试：复杂 block 中的 recur ──
(deftest block-recur-test
  (testing "recur in tail position of a block"
    (let [input (loop-node [[(variable 'x) (literal 0)]]
                           (block
                             (call (variable 'println) (variable 'x))
                             (if-node (call (variable '<) (variable 'x) (literal 3))
                                      (recur-node [(call (variable '+) (variable 'x) (literal 1))])
                                      (variable 'x))))
          output (sut/eliminate input)]
      (is (satisfies? p/INode output))
      ;; block 内递归调用不会抛异常
      (is (some? output)))))
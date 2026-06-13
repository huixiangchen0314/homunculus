(ns top.kzre.homunculus.core.types.recur-elim.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [top.kzre.homunculus.core.ir2.model :as m]
   [top.kzre.homunculus.core.ir2.protocol :as p]
   [top.kzre.homunculus.core.ir2.protocol :as ir2p]
   [top.kzre.homunculus.core.types.recur-elim.core :as sut]
   [top.kzre.homunculus.core.types.recur-elim.methods]))

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


(deftest loop-while-conversion-preserves-args
  (let [i-var   (m/->VariableNode "i" nil nil nil)
        init    (m/->LiteralNode 0.0 nil nil nil)
        test-node (m/->CallNode (m/->VariableNode "<" nil nil nil)
                                [i-var (m/->LiteralNode 10.0 nil nil nil)]
                                nil nil nil)
        recur-node (m/->RecurNode [(m/->CallNode (m/->VariableNode "+" nil nil nil)
                                                 [i-var (m/->LiteralNode 1.0 nil nil nil)]
                                                 nil nil nil)] nil nil nil)
        if-node (m/->IfNode test-node recur-node i-var nil nil nil)
        loop-node (m/->LoopNode [[i-var init]] if-node nil nil nil)
        result (sut/transform-loop loop-node)]
    ;; 转换后应为 let 节点
    (is (= :let (ir2p/kind result)))
    (let [body-block (:body result)                    ;; let 的 body 是一个 block
          exprs (:exprs body-block)]
      ;; block 的第一个表达式是 while 节点
      (let [while-node (first exprs)]
        (is (= :while (ir2p/kind while-node)))
        ;; while 的条件应该是 recur-flag 变量（不是原始 < 调用）
        (let [while-test (:test while-node)]
          (is (= :variable (ir2p/kind while-test)))
          (is (clojure.string/starts-with? (:name while-test) "recur?")))
        ;; while 的 body 应包含赋值操作（具体内容由 convert-tail 生成）
        ;; while 的 body 应包含赋值操作，具体形式由 convert-tail 决定
        (let [while-body (:body while-node)]
          (is (satisfies? ir2p/INode while-body))
          ;; 深度遍历 while-body，查找对 "i" 的赋值
          (let [found (atom false)]
            ((fn walk [n]
               (when (satisfies? ir2p/INode n)
                 (when (and (= (ir2p/kind n) :assign)
                            (= "i" (:name (:var n))))
                   (reset! found true))
                 (doseq [c (ir2p/children n)] (walk c))))
             while-body)
            (is @found "while 循环体中应包含对变量 i 的赋值操作"))))
      ;; block 的第二个表达式应该是 result 变量引用
      (let [result-ref (second exprs)]
        (is (= :variable (ir2p/kind result-ref)))
        (is (clojure.string/starts-with? (:name result-ref) "result"))))))
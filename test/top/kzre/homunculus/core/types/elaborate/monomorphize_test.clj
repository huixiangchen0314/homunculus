(ns top.kzre.homunculus.core.types.elaborate.monomorphize-test
  "单态化引擎的单元测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.elaborate.methods]
            [top.kzre.homunculus.core.types.elaborate.protocol :as cfg]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- lam [params body] (m/->LambdaNode params body [] nil nil nil nil))
(defn- call [fn-node & args] (m/->CallNode fn-node (vec args) nil nil nil))
(defn- define [name val] (m/->DefineNode name val nil nil nil nil))

(defrecord TestConfig []
  cfg/IElaborateConfig
  (max-iterations [_] 5)
  (strict-mode? [_] true)
  (allow-return-closure? [_] false)
  (on-unresolved [_ lambda] (throw (ex-info "Unresolved closure" {:lambda lambda})))
  (should-inline? [_ _ _] false))   ;; 单态化测试中不希望内联，而是触发单态化

(deftest monomorphize-id-function-test
  (testing "单态化：恒等函数传递"
    (let [config (->TestConfig)
          g-body (call (vref "f") (vref "x"))
          g-lambda (lam [(vref "f") (vref "x")] g-body)
          def-g (define 'g g-lambda)
          closure (lam [(vref "a")] (vref "a"))
          call-node (call (vref "g") closure (lit 42))
          roots [def-g call-node]
          result (elaborate/elaborate roots config)]
      ;; 闭包已消除
      (is (empty? (filter #(and (satisfies? ir2p/INode %) (= (ir2p/kind %) :lambda)) result))
          "闭包应被消除")
      ;; 应有特化函数和提升闭包定义
      (let [defines (filter #(= (ir2p/kind %) :define) result)]
        (is (>= (count defines) 3) "应有至少三个 define（原 g、提升闭包、特化函数）"))
      ;; 调用点应该指向特化函数
      (let [calls (filter #(= (ir2p/kind %) :call) result)]
        (is (= 1 (count calls)) "应有一个调用点")
        (let [new-call (first calls)
              fn-ref (:fn new-call)]
          (is (= (ir2p/kind fn-ref) :variable)
              "调用函数应为变量引用")
          (is (not= "g" (:name fn-ref))
              "调用函数不应是原函数 g，应为特化后的版本"))))))

(deftest monomorphize-with-free-vars-test
  (testing "单态化：带自由变量的闭包"
    (let [config (->TestConfig)
          add-body (call (vref "f") (vref "x"))
          add-lambda (lam [(vref "f") (vref "x")] add-body)
          def-add (define 'add add-lambda)
          closure-body (call (vref "+") (vref "a") (vref "n"))
          closure (lam [(vref "a")] closure-body)
          call-node (call (vref "add") closure (lit 5))
          def-n (define 'n (lit 10))
          roots [def-n def-add call-node]
          result (elaborate/elaborate roots config)]
      ;; 闭包已消除
      (is (empty? (filter #(and (satisfies? ir2p/INode %) (= (ir2p/kind %) :lambda)) result))
          "闭包应被消除")
      ;; 应有特化函数和提升闭包定义，以及 n 和 add 的定义
      (let [defines (filter #(= (ir2p/kind %) :define) result)]
        (is (>= (count defines) 4) "应有至少四个 define（n、add、提升闭包、特化函数）"))
      ;; 调用点应该指向特化函数
      (let [calls (filter #(= (ir2p/kind %) :call) result)]
        (is (= 1 (count calls)) "应有一个调用点")
        (let [new-call (first calls)
              fn-ref (:fn new-call)]
          (is (= (ir2p/kind fn-ref) :variable)
              "调用函数应为变量引用")
          (is (not= "add" (:name fn-ref))
              "调用函数不应是原 add，应为特化后的版本"))))))
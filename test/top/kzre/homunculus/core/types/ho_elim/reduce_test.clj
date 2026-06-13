(ns top.kzre.homunculus.core.types.ho-elim.reduce-test
  "测试高阶消除 pass 对 reduce 的展开。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]))

;; 简单的配置：只识别 reduce
(def reduce-config
  (reify hop/IHoElimConfig
    (known-ho-functions [_] {'reduce :reduce})))

;; 辅助函数
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- call [f & args] (m/->CallNode f (vec args) nil nil nil))
(defn- vec-node [items] (m/->VectorNode items nil nil nil))

(deftest test-reduce-empty
  (testing "reduce with empty vector returns init"
    (let [init (lit 0)
          f    (vref "+")
          coll (vec-node [])
          reduce-call (call (vref "reduce") f init coll)
          result (ho-elim/eliminate reduce-call reduce-config)]
      (is (identical? init result) "空向量应直接返回初始值"))))

(deftest test-reduce-single
  (testing "reduce with single element vector"
    (let [init (lit 0)
          f    (vref "+")
          coll (vec-node [(lit 1)])
          reduce-call (call (vref "reduce") f init coll)
          result (ho-elim/eliminate reduce-call reduce-config)]
      (is (= :call (node/kind result)))
      (is (= f (node/call-fn result)))
      (let [args (node/call-args result)]
        (is (= 2 (count args)))
        (is (= init (first args)))
        (is (= (lit 1) (second args)))))))

(deftest test-reduce-multiple
  (testing "reduce with multiple elements"
    (let [init (lit 0)
          f    (vref "+")
          coll (vec-node [(lit 1) (lit 2) (lit 3)])
          reduce-call (call (vref "reduce") f init coll)
          result (ho-elim/eliminate reduce-call reduce-config)]
      ;; 应生成嵌套调用 (+ (+ (+ 0 1) 2) 3)
      (is (= :call (node/kind result)))
      (let [outer (node/call-args result)]
        (is (= 2 (count outer)))
        ;; 第二个参数是字面量 3
        (is (= (lit 3) (second outer)))
        ;; 第一个参数是另一个调用 (+ (+ 0 1) 2)
        (let [inner (first outer)]
          (is (= :call (node/kind inner)))
          (let [inner-args (node/call-args inner)]
            (is (= 2 (count inner-args)))
            (is (= (lit 2) (second inner-args)))
            (let [inner-inner (first inner-args)]
              (is (= :call (node/kind inner-inner)))
              (let [inner-inner-args (node/call-args inner-inner)]
                (is (= 2 (count inner-inner-args)))
                (is (= init (first inner-inner-args)))
                (is (= (lit 1) (second inner-inner-args)))))))))))
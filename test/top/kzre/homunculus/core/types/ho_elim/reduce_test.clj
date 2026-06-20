(ns top.kzre.homunculus.core.types.ho-elim.reduce-test
  "测试高阶消除 pass 对 reduce 的展开。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
            [top.kzre.homunculus.core.types.model :as t]))

(def reduce-config
  (reify hop/IHoElimConfig
    (known-ho-functions [_] {'reduce :reduce})
    (supports-dynamic-collections? [_] false)
    (backend-length-fn [_] 'count)
    (backend-nth-fn [_] 'nth)
    (backend-less-than-fn [_] '<)))

(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- call [f & args] (m/->CallNode f (vec args) nil nil nil))

(defn- typed-vec [items]
  (let [elem-ty (t/->TCon :int)
        shape   (t/->FixedLength (count items))
        ty      (t/->TVec :vector elem-ty shape)]
    (m/->VectorNode items {:type ty} nil nil)))

(deftest test-reduce-empty
  (let [init (lit 0)
        f    (vref "+")
        coll (typed-vec [])
        reduce-call (call (vref "reduce") f init coll)
        result (ho-elim/eliminate reduce-call reduce-config)]
    (is (identical? init result) "空向量应直接返回初始值")))

(deftest test-reduce-single
  (let [init (lit 0)
        f    (vref "+")
        coll (typed-vec [(lit 1)])
        reduce-call (call (vref "reduce") f init coll)
        result (ho-elim/eliminate reduce-call reduce-config)]
    (is (= :call (node/kind result)))
    (is (= f (node/call-fn result)))
    (let [args (node/call-args result)]
      (is (= 2 (count args)))
      (is (= init (first args)))
      (is (= (lit 1) (second args))))))

(deftest test-reduce-multiple
  (let [init (lit 0)
        f    (vref "+")
        coll (typed-vec [(lit 1) (lit 2) (lit 3)])
        reduce-call (call (vref "reduce") f init coll)
        result (ho-elim/eliminate reduce-call reduce-config)]
    (is (= :call (node/kind result)))
    (let [outer (node/call-args result)]
      (is (= 2 (count outer)))
      (is (= (lit 3) (second outer)))
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
              (is (= (lit 1) (second inner-inner-args))))))))))

(deftest test-reduce-non-vector
  (let [init (lit 0)
        f    (vref "+")
        coll (vref "some-list")
        reduce-call (call (vref "reduce") f init coll)
        result (ho-elim/eliminate reduce-call reduce-config)]
    (is (= :call (node/kind result)))
    (is (= "reduce" (node/var-name (node/call-fn result))))
    (is (= 3 (count (node/call-args result))))))
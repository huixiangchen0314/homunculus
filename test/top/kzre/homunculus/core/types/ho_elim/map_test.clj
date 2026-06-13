(ns top.kzre.homunculus.core.types.ho-elim.map-test
  "测试高阶消除 pass 对 map 的展开。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]))

(def map-config
  (reify hop/IHoElimConfig
    (known-ho-functions [_] {'map :map})
    (supports-dynamic-collections? [_] false)
    (backend-length-fn [_] 'count)
    (backend-nth-fn [_] 'nth)
    (backend-less-than-fn [_] '<)))

(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- call [f & args] (m/->CallNode f (vec args) nil nil nil))
(defn- vec-node [items] (m/->VectorNode items nil nil nil))

(deftest test-map-empty
  (testing "map over empty vector"
    (let [f    (vref "inc")
          coll (vec-node [])
          map-call (call (vref "map") f coll)
          result (ho-elim/eliminate map-call map-config)]
      (is (= :vector (node/kind result)))
      (is (empty? (node/vec-items result))))))

(deftest test-map-single
  (testing "map over single element"
    (let [f    (vref "inc")
          coll (vec-node [(lit 1)])
          map-call (call (vref "map") f coll)
          result (ho-elim/eliminate map-call map-config)]
      (is (= :vector (node/kind result)))
      (let [items (node/vec-items result)]
        (is (= 1 (count items)))
        (let [item (first items)]
          (is (= :call (node/kind item)))
          (is (= f (node/call-fn item)))
          (is (= [(lit 1)] (node/call-args item))))))))

(deftest test-map-multiple
  (testing "map over multiple elements"
    (let [f    (vref "inc")
          coll (vec-node [(lit 1) (lit 2) (lit 3)])
          map-call (call (vref "map") f coll)
          result (ho-elim/eliminate map-call map-config)]
      (is (= :vector (node/kind result)))
      (let [items (node/vec-items result)]
        (is (= 3 (count items)))
        (is (every? #(= :call (node/kind %)) items))
        (is (= [(lit 1)] (node/call-args (first items))))
        (is (= [(lit 2)] (node/call-args (second items))))
        (is (= [(lit 3)] (node/call-args (nth items 2))))))))

(deftest test-map-non-vector
  (testing "map over non-vector leaves call unchanged"
    (let [f    (vref "inc")
          coll (vref "dynamic-list")
          map-call (call (vref "map") f coll)
          result (ho-elim/eliminate map-call map-config)]
      (is (= :call (node/kind result)))
      (is (= "map" (node/var-name (node/call-fn result))))
      (is (= 2 (count (node/call-args result)))))))
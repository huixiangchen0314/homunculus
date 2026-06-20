(ns top.kzre.homunculus.core.types.ho-elim.core-test
  "高阶消除 pass 核心测试：验证 reduce、map 等展开逻辑。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
            [top.kzre.homunculus.core.types.model :as t]))

(def fixed-config
  (reify hop/IHoElimConfig
    (known-ho-functions [_] {'reduce :reduce, 'map :map})
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

;; ══════════════════════════════════════════════
;; Reduce
;; ══════════════════════════════════════════════
(deftest reduce-empty
  (let [init (lit 0)
        f    (vref "+")
        coll (typed-vec [])
        r    (call (vref "reduce") f init coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (identical? init res))))

(deftest reduce-single
  (let [init (lit 0)
        f    (vref "+")
        coll (typed-vec [(lit 1)])
        r    (call (vref "reduce") f init coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :call (node/kind res)))
    (is (= f (node/call-fn res)))
    (let [args (node/call-args res)]
      (is (= 2 (count args)))
      (is (= init (first args)))
      (is (= (lit 1) (second args))))))

(deftest reduce-multiple
  (let [init (lit 0)
        f    (vref "+")
        coll (typed-vec [(lit 1) (lit 2) (lit 3)])
        r    (call (vref "reduce") f init coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :call (node/kind res)))
    (let [args (node/call-args res)]
      (is (= 2 (count args)))
      (is (= (lit 3) (second args)))
      (let [inner (first args)]
        (is (= :call (node/kind inner)))
        (let [iargs (node/call-args inner)]
          (is (= 2 (count iargs)))
          (is (= (lit 2) (second iargs)))
          (let [inner2 (first iargs)]
            (is (= :call (node/kind inner2)))
            (let [i2args (node/call-args inner2)]
              (is (= 2 (count i2args)))
              (is (= init (first i2args)))
              (is (= (lit 1) (second i2args))))))))))

(deftest reduce-non-vector
  (let [init (lit 0)
        f    (vref "+")
        coll (vref "unknown")
        r    (call (vref "reduce") f init coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :call (node/kind res)))
    (is (= "reduce" (node/var-name (node/call-fn res))))
    (is (= 3 (count (node/call-args res))))))

;; ══════════════════════════════════════════════
;; Map
;; ══════════════════════════════════════════════
(deftest map-empty
  (let [f    (vref "inc")
        coll (typed-vec [])
        r    (call (vref "map") f coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :vector (node/kind res)))
    (is (empty? (node/vec-items res)))))

(deftest map-single
  (let [f    (vref "inc")
        coll (typed-vec [(lit 1)])
        r    (call (vref "map") f coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :vector (node/kind res)))
    (let [items (node/vec-items res)]
      (is (= 1 (count items)))
      (let [item (first items)]
        (is (= :call (node/kind item)))
        (is (= f (node/call-fn item)))
        (is (= [(lit 1)] (node/call-args item)))))))

(deftest map-multiple
  (let [f    (vref "inc")
        coll (typed-vec [(lit 1) (lit 2) (lit 3)])
        r    (call (vref "map") f coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :vector (node/kind res)))
    (let [items (node/vec-items res)]
      (is (= 3 (count items)))
      (is (every? #(= :call (node/kind %)) items))
      (is (= [(lit 1)] (node/call-args (first items))))
      (is (= [(lit 2)] (node/call-args (second items))))
      (is (= [(lit 3)] (node/call-args (nth items 2)))))))

(deftest map-non-vector
  (let [f    (vref "inc")
        coll (vref "dynamic")
        r    (call (vref "map") f coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :call (node/kind res)))
    (is (= "map" (node/var-name (node/call-fn res))))
    (is (= 2 (count (node/call-args res))))))

;; ══════════════════════════════════════════════
;; 配置相关
;; ══════════════════════════════════════════════
(deftest unknown-ho-function
  (let [r   (call (vref "filter") (vref "odd?") (typed-vec []))
        res (ho-elim/eliminate r fixed-config)]
    (is (= :call (node/kind res)))
    (is (= "filter" (node/var-name (node/call-fn res))))))

(deftest dynamic-collection-throws
  (let [dyn-config (reify hop/IHoElimConfig
                     (known-ho-functions [_] {'reduce :reduce})
                     (supports-dynamic-collections? [_] false)
                     (backend-length-fn [_] 'count)
                     (backend-nth-fn [_] 'nth)
                     (backend-less-than-fn [_] '<))
        r   (call (vref "reduce") (vref "+") (lit 0) (vref "xs"))
        res (ho-elim/eliminate r dyn-config)]
    (is (= :call (node/kind res)))
    (is (= "reduce" (node/var-name (node/call-fn res))))))
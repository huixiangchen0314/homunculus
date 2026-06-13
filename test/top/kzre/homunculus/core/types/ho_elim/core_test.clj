(ns top.kzre.homunculus.core.types.ho-elim.core-test
  "高阶消除 pass 核心测试：验证 reduce、map 等展开逻辑。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]))

;; ── 配置：固定长度展开，不支持动态 ──
(def fixed-config
  (reify hop/IHoElimConfig
    (known-ho-functions [_] {'reduce :reduce, 'map :map})
    (supports-dynamic-collections? [_] false)
    (backend-length-fn [_] 'count)
    (backend-nth-fn [_] 'nth)
    (backend-less-than-fn [_] '<)))

;; ── 辅助函数 ──
(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- call [f & args] (m/->CallNode f (vec args) nil nil nil))
(defn- vec-node [items] (m/->VectorNode items nil nil nil))

;; ══════════════════════════════════════════════
;; Reduce 测试
;; ══════════════════════════════════════════════
(deftest reduce-empty
  (let [init (lit 0)
        f    (vref "+")
        coll (vec-node [])
        r    (call (vref "reduce") f init coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (identical? init res) "空向量应直接返回初始值")))

(deftest reduce-single
  (let [init (lit 0)
        f    (vref "+")
        coll (vec-node [(lit 1)])
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
        coll (vec-node [(lit 1) (lit 2) (lit 3)])
        r    (call (vref "reduce") f init coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :call (node/kind res)))
    (let [args (node/call-args res)]
      (is (= 2 (count args)))
      (is (= (lit 3) (second args)))            ;; (+ (+ (+ 0 1) 2) 3)
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
;; Map 测试
;; ══════════════════════════════════════════════
(deftest map-empty
  (let [f    (vref "inc")
        coll (vec-node [])
        r    (call (vref "map") f coll)
        res  (ho-elim/eliminate r fixed-config)]
    (is (= :vector (node/kind res)))
    (is (empty? (node/vec-items res)))))

(deftest map-single
  (let [f    (vref "inc")
        coll (vec-node [(lit 1)])
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
        coll (vec-node [(lit 1) (lit 2) (lit 3)])
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
;; 配置相关测试
;; ══════════════════════════════════════════════
(deftest unknown-ho-function
  (let [r   (call (vref "filter") (vref "odd?") (vec-node []))
        res (ho-elim/eliminate r fixed-config)]
    (is (= :call (node/kind res))                ;; 未注册的函数保持不变
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
      ;; 由于不支持动态，且 coll 不是 vector，应保留原调用
      (is (= :call (node/kind res)))
      (is (= "reduce" (node/var-name (node/call-fn res)))))))
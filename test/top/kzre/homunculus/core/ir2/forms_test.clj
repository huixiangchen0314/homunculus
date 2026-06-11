(ns top.kzre.homunculus.core.ir2.forms-test
  "测试 IR2 特殊形式：if, do, let, fn, def, loop, recur, quote 等。
   依赖 ir2.core 和 ir2.forms 注册方法。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]))

(def kind-key ::ir2/kind)

(defn- node?
  [x expected-kind]
  (and (map? x) (= expected-kind (get x kind-key))))

(defn- literal-node?
  [x val]
  (and (node? x :literal) (= val (:val x))))

(defn- var-node?
  [x sym]
  (and (node? x :var) (= sym (:name x))))

(defn- prim-node?
  [x op]
  (and (node? x :prim) (= op (:op x))))

(defn- ir->node [ir-vec] (first ir-vec))

;; ── If 测试 ─────────────────────────────────
(deftest if-lowering-test
  (testing "if with both branches"
    (let [ir2 (-> (ir1/->ir1 '(if (> x 0) "pos" "neg")) ir2/lower)]
      (is (= 4 (count ir2)))
      (is (node? (ir->node ir2) :if))
      (is (prim-node? (ir->node (second ir2)) :gt))
      (is (literal-node? (ir->node (nth ir2 2)) "pos"))
      (is (literal-node? (ir->node (nth ir2 3)) "neg"))))
  (testing "if without else (此测试实际测试乘法，旧版遗留，保留但修正)"
    (let [ir2 (-> (ir1/->ir1 '(* (+ 1 2) 3)) ir2/lower)]
      (is (= 3 (count ir2)))
      (is (prim-node? (ir->node ir2) :mul))
      (let [inner (second ir2)]
        (is (prim-node? (ir->node inner) :add))
        (is (= 3 (count inner)))
        (is (literal-node? (ir->node (second inner)) 1))
        (is (literal-node? (ir->node (nth inner 2)) 2)))
      (is (literal-node? (ir->node (nth ir2 2)) 3)))))

;; ── Do 测试 ─────────────────────────────────
(deftest do-lowering-test
  (testing "do 表达式被 lowering 为 :do"
    (let [ir2 (-> (ir1/->ir1 '(do 1 2 3)) ir2/lower)]
      (is (= 4 (count ir2)))
      (is (node? (ir->node ir2) :do))
      (is (literal-node? (ir->node (second ir2)) 1))
      (is (literal-node? (ir->node (nth ir2 2)) 2))
      (is (literal-node? (ir->node (nth ir2 3)) 3)))))

;; ── Let 测试 ────────────────────────────────
(deftest let-lowering-test
  (testing "let 绑定降低为 :let"
    (let [ir2 (-> (ir1/->ir1 '(let* [x 1 y 2] (+ x y))) ir2/lower)]
      (is (= 6 (count ir2)))
      (is (node? (ir->node ir2) :let))
      (is (var-node? (ir->node (second ir2)) 'x))
      (is (literal-node? (ir->node (nth ir2 2)) 1))
      (is (var-node? (ir->node (nth ir2 3)) 'y))
      (is (literal-node? (ir->node (nth ir2 4)) 2))
      (is (prim-node? (ir->node (nth ir2 5)) :add)))))

;; ── Fn 测试 ────────────────────────────────
(deftest fn-lowering-test
  (testing "匿名函数降低为 :fn"
    (let [ir2 (-> (ir1/->ir1 '(fn* [a b] (+ a b))) ir2/lower)]
      (is (= 4 (count ir2)))
      (is (node? (ir->node ir2) :fn))
      (is (var-node? (ir->node (second ir2)) 'a))
      (is (var-node? (ir->node (nth ir2 2)) 'b))
      (is (prim-node? (ir->node (nth ir2 3)) :add))))
  (testing "具名函数降低包含名称"
    (let [ir2 (-> (ir1/->ir1 '(fn* my-add [a b] (+ a b))) ir2/lower)]
      (is (= 5 (count ir2)))
      (is (node? (ir->node ir2) :fn))
      (is (var-node? (ir->node (second ir2)) 'my-add))
      (is (var-node? (ir->node (nth ir2 2)) 'a))
      (is (var-node? (ir->node (nth ir2 3)) 'b))
      (is (prim-node? (ir->node (nth ir2 4)) :add)))))

;; ── Def 测试 ────────────────────────────────
(deftest def-lowering-test
  (testing "带值的 def 降低为 :def"
    (let [ir2 (-> (ir1/->ir1 '(def x 42)) ir2/lower)]
      (is (= 3 (count ir2)))
      (is (node? (ir->node ir2) :def))
      (is (var-node? (ir->node (second ir2)) 'x))
      (is (literal-node? (ir->node (nth ir2 2)) 42))))
  (testing "无值的 def"
    (let [ir2 (-> (ir1/->ir1 '(def x)) ir2/lower)]
      (is (= 2 (count ir2)))
      (is (node? (ir->node ir2) :def))
      (is (var-node? (ir->node (second ir2)) 'x)))))

;; ── Loop / Recur 测试 ──────────────────────
(deftest loop-lowering-test
  (testing "loop 降低为 :loop"
    (let [ir2 (-> (ir1/->ir1 '(loop* [x 0] (if (< x 10) (recur (inc x)) x))) ir2/lower)]
      (is (= 4 (count ir2)))
      (is (node? (ir->node ir2) :loop))
      (is (var-node? (ir->node (second ir2)) 'x))
      (is (literal-node? (ir->node (nth ir2 2)) 0))
      (is (node? (ir->node (nth ir2 3)) :if)))))

(deftest recur-lowering-test
  (testing "recur 降低为 :recur"
    (let [ir2 (-> (ir1/->ir1 '(recur (inc x))) ir2/lower)]
      (is (= 2 (count ir2)))
      (is (node? (ir->node ir2) :recur))
      (is (prim-node? (ir->node (second ir2)) :inc)))))

;; ── Quote 测试 ─────────────────────────────
(deftest quote-lowering-test
  (testing "quote 符号"
    (let [ir2 (-> (ir1/->ir1 '(quote x)) ir2/lower)]
      (is (= 2 (count ir2)))
      (is (node? (ir->node ir2) :quote))
      (is (var-node? (ir->node (second ir2)) 'x))))
  (testing "quote 列表内部仍会被解析为调用"
    (let [ir2 (-> (ir1/->ir1 '(quote (1 2 3))) ir2/lower)]
      (is (node? (ir->node ir2) :quote))
      (is (node? (ir->node (second ir2)) :call)))))
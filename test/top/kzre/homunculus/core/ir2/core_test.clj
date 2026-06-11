(ns top.kzre.homunculus.core.ir2.core-test
  "测试 IR2 基础节点：字面量、符号、调用、向量、映射等。
   依赖 ir2.core 和 ir2.forms 注册方法。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]           ;; 加载 ir1 特殊形式
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]))         ;; 加载 ir2 特殊形式

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

(deftest literal-lowering-test
  (testing "数字字面量"
    (let [ir2 (-> (ir1/->ir1 42) ir2/lower)]
      (is (= 1 (count ir2)))
      (let [node (ir->node ir2)]
        (is (node? node :literal))
        (is (= 42 (:val node)))
        (is (nil? (:attrs node))))))          ;; 不再推导类型
  (testing "字符串字面量"
    (let [ir2 (-> (ir1/->ir1 "hello") ir2/lower)]
      (let [node (ir->node ir2)]
        (is (literal-node? node "hello"))
        (is (nil? (:attrs node))))))
  (testing "布尔字面量"
    (let [ir2 (-> (ir1/->ir1 true) ir2/lower)]
      (is (literal-node? (ir->node ir2) true))
      (is (nil? (:attrs (ir->node ir2)))))
    (let [ir2 (-> (ir1/->ir1 false) ir2/lower)]
      (is (literal-node? (ir->node ir2) false))))
  (testing "nil 字面量"
    (let [ir2 (-> (ir1/->ir1 nil) ir2/lower)]
      (is (literal-node? (ir->node ir2) nil))))
  (testing "关键字字面量"
    (let [ir2 (-> (ir1/->ir1 :foo) ir2/lower)]
      (is (literal-node? (ir->node ir2) :foo))
      (is (nil? (:attrs (ir->node ir2))))))
  (testing "字符字面量"
    (let [ir2 (-> (ir1/->ir1 \a) ir2/lower)]
      (is (literal-node? (ir->node ir2) \a))
      (is (nil? (:attrs (ir->node ir2)))))))

(deftest symbol-to-var-test
  (testing "简单符号被 lowering 为 :var"
    (let [ir2 (-> (ir1/->ir1 'x) ir2/lower)]
      (is (= 1 (count ir2)))
      (is (var-node? (ir->node ir2) 'x))))
  (testing "带命名空间的符号"
    (let [sym 'clojure.core/+]
      (let [ir2 (-> (ir1/->ir1 sym) ir2/lower)]
        (is (var-node? (ir->node ir2) sym))))))

(deftest prim-call-test
  (testing "加法操作被 lowering 为 :prim :add"
    (let [ir2 (-> (ir1/->ir1 '(+ 1 2)) ir2/lower)]
      (is (= 3 (count ir2)))
      (is (prim-node? (ir->node ir2) :add))
      (is (literal-node? (ir->node (second ir2)) 1))
      (is (literal-node? (ir->node (nth ir2 2)) 2))))
  (testing "一元操作 not"
    (let [ir2 (-> (ir1/->ir1 '(not false)) ir2/lower)]
      (is (= 2 (count ir2)))
      (is (prim-node? (ir->node ir2) :not))
      (is (literal-node? (ir->node (second ir2)) false))))
  (testing "嵌套原语"
    (let [ir2 (-> (ir1/->ir1 '(* (+ 1 2) 3)) ir2/lower)]
      (is (= 3 (count ir2)))
      (is (prim-node? (ir->node ir2) :mul))
      (let [inner (second ir2)]
        (is (prim-node? (ir->node inner) :add))
        (is (literal-node? (ir->node (second inner)) 1))
        (is (literal-node? (ir->node (nth inner 2)) 2)))
      (is (literal-node? (ir->node (nth ir2 2)) 3))))
  (testing "println 被 lowering 为 :print"
    (let [ir2 (-> (ir1/->ir1 '(println "hello")) ir2/lower)]
      (is (prim-node? (ir->node ir2) :print))
      (is (literal-node? (ir->node (second ir2)) "hello")))))

(deftest call-lowering-test
  (testing "非原语调用被 lowering 为 :call"
    (let [ir2 (-> (ir1/->ir1 '(my-fn 10)) ir2/lower)]
      (is (= 3 (count ir2)))
      (is (node? (ir->node ir2) :call))
      (let [fn-ir (second ir2)]
        (is (var-node? (ir->node fn-ir) 'my-fn)))
      (is (literal-node? (ir->node (nth ir2 2)) 10)))))

(deftest vector-lowering-test
  (testing "向量保留 :vector 节点但子元素被 lowering"
    (let [ir2 (-> (ir1/->ir1 '[1 2]) ir2/lower)]
      (is (= 3 (count ir2)))
      (is (node? (ir->node ir2) :vector))
      (is (literal-node? (ir->node (second ir2)) 1))
      (is (literal-node? (ir->node (nth ir2 2)) 2)))))

(deftest map-lowering-test
  (testing "映射保留 :map 节点且键值被 lowering"
    (let [ir2 (-> (ir1/->ir1 '{:a 1 :b 2}) ir2/lower)]
      (is (= 5 (count ir2)))
      (is (node? (ir->node ir2) :map))
      (is (literal-node? (ir->node (second ir2)) :a))
      (is (literal-node? (ir->node (nth ir2 2)) 1))
      (is (literal-node? (ir->node (nth ir2 3)) :b))
      (is (literal-node? (ir->node (nth ir2 4)) 2)))))
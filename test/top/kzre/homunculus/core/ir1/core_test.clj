(ns top.kzre.homunculus.core.ir1.core-test
  "测试 IR1 基础节点：字面量、符号、调用、向量、映射等。
   依赖 ir1.core 和 ir1.forms 注册方法。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]))  ;; 触发方法注册

(def kind-key ::ir1/kind)

(defn- node?
  [x expected-kind]
  (and (map? x) (= expected-kind (get x kind-key))))

(defn- literal-node?
  [x val]
  (and (node? x :literal) (= val (:val x))))

(defn- symbol-node?
  [x sym]
  (and (node? x :symbol) (= sym (:name x))))

(defn- ir->node [ir-vec] (first ir-vec))

(deftest literal-test
  (testing "数字字面量"
    (let [ir (ir1/->ir1 42)]
      (is (= 1 (count ir)))
      (is (literal-node? (ir->node ir) 42))))
  (testing "字符串字面量"
    (let [ir (ir1/->ir1 "hello")]
      (is (= 1 (count ir)))
      (is (literal-node? (ir->node ir) "hello"))))
  (testing "布尔字面量 true"
    (let [ir (ir1/->ir1 true)]
      (is (literal-node? (ir->node ir) true))))
  (testing "布尔字面量 false"
    (let [ir (ir1/->ir1 false)]
      (is (literal-node? (ir->node ir) false))))
  (testing "nil 字面量"
    (let [ir (ir1/->ir1 nil)]
      (is (literal-node? (ir->node ir) nil))))
  (testing "关键字字面量"
    (let [ir (ir1/->ir1 :foo)]
      (is (literal-node? (ir->node ir) :foo))))
  (testing "字符字面量"
    (let [ir (ir1/->ir1 \a)]
      (is (literal-node? (ir->node ir) \a)))))

(deftest symbol-test
  (testing "简单符号"
    (let [ir (ir1/->ir1 'x)]
      (is (= 1 (count ir)))
      (is (symbol-node? (ir->node ir) 'x))))
  (testing "带命名空间的符号"
    (let [sym 'clojure.core/+]
      (let [ir (ir1/->ir1 sym)]
        (is (symbol-node? (ir->node ir) sym))))))

(deftest call-test
  (testing "一元函数调用"
    (let [ir (ir1/->ir1 '(not true))]
      (is (= 3 (count ir)))
      (let [node (ir->node ir)]
        (is (node? node :call))
        (is (= 'not (:op node))))
      (is (symbol-node? (ir->node (second ir)) 'not))
      (is (literal-node? (ir->node (nth ir 2)) true))))
  (testing "二元函数调用"
    (let [ir (ir1/->ir1 '(+ 1 2))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :call))
      (is (= '+ (:op (ir->node ir))))
      (is (symbol-node? (ir->node (second ir)) '+))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (is (literal-node? (ir->node (nth ir 3)) 2))))
  (testing "嵌套函数调用"
    (let [ir (ir1/->ir1 '(+ 1 (* 2 3)))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :call))
      (is (= '+ (:op (ir->node ir))))
      (is (symbol-node? (ir->node (second ir)) '+))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (let [inner-ir (nth ir 3)
            inner-node (ir->node inner-ir)]
        (is (node? inner-node :call))
        (is (= '* (:op inner-node)))
        (is (symbol-node? (ir->node (second inner-ir)) '*))
        (is (literal-node? (ir->node (nth inner-ir 2)) 2))
        (is (literal-node? (ir->node (nth inner-ir 3)) 3))))))

(deftest vector-test
  (testing "空向量"
    (let [ir (ir1/->ir1 [])]
      (is (= 1 (count ir)))
      (is (node? (ir->node ir) :vector))
      (is (empty? (:items (ir->node ir))))))
  (testing "包含元素的向量"
    (let [ir (ir1/->ir1 [1 'x "hi"])]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :vector))
      (is (= 3 (count (:items (ir->node ir)))))
      (is (literal-node? (ir->node (second ir)) 1))
      (is (symbol-node? (ir->node (nth ir 2)) 'x))
      (is (literal-node? (ir->node (nth ir 3)) "hi"))))
  (testing "嵌套向量"
    (let [ir (ir1/->ir1 [[1 2] 3])]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :vector))
      (let [inner-ir (second ir)
            inner-node (ir->node inner-ir)]
        (is (vector? inner-ir))
        (is (node? inner-node :vector))
        (is (literal-node? (ir->node (second inner-ir)) 1))
        (is (literal-node? (ir->node (nth inner-ir 2)) 2)))
      (is (literal-node? (ir->node (nth ir 2)) 3)))))

(deftest map-test
  (testing "空映射"
    (let [ir (ir1/->ir1 {})]
      (is (= 1 (count ir)))
      (is (node? (ir->node ir) :map))))
  (testing "简单映射"
    (let [ir (ir1/->ir1 {:a 1 :b 2})]
      (is (= 5 (count ir)))
      (is (node? (ir->node ir) :map))
      (is (literal-node? (ir->node (second ir)) :a))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (is (literal-node? (ir->node (nth ir 3)) :b))
      (is (literal-node? (ir->node (nth ir 4)) 2))))
  (testing "映射键为表达式"
    (let [ir (ir1/->ir1 '{(inc 1) (+ 2 3)})]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :map))
      (is (node? (ir->node (second ir)) :call))
      (is (node? (ir->node (nth ir 2)) :call)))))

(deftest unsupported-form-test
  (testing "不支持的类型（如 Java 对象）"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ir1/->ir1 (java.util.Date.)))))
  (testing "不支持的类型（如正则表达式）"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ir1/->ir1 #"foo")))))
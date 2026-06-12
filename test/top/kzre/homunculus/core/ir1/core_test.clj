(ns top.kzre.homunculus.core.ir1.core-test
  "IR1 核心测试，基于 defrecord 节点和协议。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]
            [top.kzre.homunculus.core.ir1.protocol :as p]
            [top.kzre.homunculus.core.ir1.forms]))   ;; 加载特殊形式方法

;; ── 辅助函数 ─────────────────────────────────
(defn- node? [node expected-kind]
  (and (satisfies? p/INode node)
       (= expected-kind (p/kind node))))

(defn- literal-node? [node val]
  (and (node? node :literal)
       (= val (:val node))))

(defn- symbol-node? [node sym]
  (and (node? node :symbol)
       (= sym (:name node))))

;; ── 字面量测试 ─────────────────────────────
(deftest literal-test
  (testing "数字字面量"
    (let [node (ir1/->ir1 42)]
      (is (literal-node? node 42))
      (is (empty? (p/children node)))))
  (testing "字符串字面量"
    (let [node (ir1/->ir1 "hello")]
      (is (literal-node? node "hello"))
      (is (empty? (p/children node)))))
  (testing "布尔字面量 true"
    (let [node (ir1/->ir1 true)]
      (is (literal-node? node true))))
  (testing "布尔字面量 false"
    (let [node (ir1/->ir1 false)]
      (is (literal-node? node false))))
  (testing "nil 字面量"
    (let [node (ir1/->ir1 nil)]
      (is (literal-node? node nil))))
  (testing "关键字字面量"
    (let [node (ir1/->ir1 :foo)]
      (is (literal-node? node :foo))))
  (testing "字符字面量"
    (let [node (ir1/->ir1 \a)]
      (is (literal-node? node \a)))))

;; ── 符号测试 ───────────────────────────────
(deftest symbol-test
  (testing "简单符号"
    (let [node (ir1/->ir1 'x)]
      (is (symbol-node? node 'x))
      (is (empty? (p/children node)))))
  (testing "带命名空间的符号"
    (let [sym 'clojure.core/+]
      (let [node (ir1/->ir1 sym)]
        (is (symbol-node? node sym))))))

;; ── 调用测试 ───────────────────────────────
(deftest call-test
  (testing "一元函数调用"
    (let [node (ir1/->ir1 '(not true))
          kids (p/children node)]
      (is (node? node :call))
      (is (= 'not (:op node)))
      (is (= 2 (count kids)))         ; op + 1 arg
      (is (symbol-node? (first kids) 'not))
      (is (literal-node? (second kids) true))))
  (testing "二元函数调用"
    (let [node (ir1/->ir1 '(+ 1 2))
          kids (p/children node)]
      (is (node? node :call))
      (is (= '+ (:op node)))
      (is (= 3 (count kids)))         ; op + 2 args
      (is (symbol-node? (first kids) '+))
      (is (literal-node? (second kids) 1))
      (is (literal-node? (nth kids 2) 2))))
  (testing "嵌套函数调用"
    (let [node (ir1/->ir1 '(+ 1 (* 2 3)))
          kids (p/children node)]
      (is (node? node :call))
      (is (= '+ (:op node)))
      (is (= 3 (count kids)))
      (is (symbol-node? (first kids) '+))
      (is (literal-node? (second kids) 1))
      (let [inner-node (nth kids 2)]
        (is (node? inner-node :call))
        (is (= '* (:op inner-node)))
        (let [inner-kids (p/children inner-node)]
          (is (= 3 (count inner-kids)))
          (is (symbol-node? (first inner-kids) '*))
          (is (literal-node? (second inner-kids) 2))
          (is (literal-node? (nth inner-kids 2) 3)))))))

;; ── 向量测试 ───────────────────────────────
(deftest vector-test
  (testing "空向量"
    (let [node (ir1/->ir1 [])]
      (is (node? node :vector))
      (is (empty? (:items node)))
      (is (empty? (p/children node)))))
  (testing "包含元素的向量"
    (let [node (ir1/->ir1 [1 'x "hi"])
          kids (p/children node)]
      (is (node? node :vector))
      (is (= 3 (count (:items node))))
      (is (= 3 (count kids)))
      (is (literal-node? (first kids) 1))
      (is (symbol-node? (second kids) 'x))
      (is (literal-node? (nth kids 2) "hi"))))
  (testing "嵌套向量"
    (let [node (ir1/->ir1 [[1 2] 3])
          kids (p/children node)]
      (is (node? node :vector))
      (is (= 2 (count kids)))
      (let [inner-node (first kids)]
        (is (node? inner-node :vector))
        (let [inner-kids (p/children inner-node)]
          (is (= 2 (count inner-kids)))
          (is (literal-node? (first inner-kids) 1))
          (is (literal-node? (second inner-kids) 2))))
      (is (literal-node? (second kids) 3)))))

;; ── 映射测试 ───────────────────────────────
(deftest map-test
  (testing "空映射"
    (let [node (ir1/->ir1 {})]
      (is (node? node :map))
      (is (empty? (p/children node)))))
  (testing "简单映射"
    (let [node (ir1/->ir1 {:a 1 :b 2})
          kids (p/children node)]
      (is (node? node :map))
      (is (= 4 (count kids)))       ; 2 keys + 2 vals
      (is (literal-node? (first kids) :a))
      (is (literal-node? (second kids) 1))
      (is (literal-node? (nth kids 2) :b))
      (is (literal-node? (nth kids 3) 2))))
  (testing "映射键为表达式"
    (let [node (ir1/->ir1 '{(inc 1) (+ 2 3)})
          kids (p/children node)]
      (is (node? node :map))
      (is (= 2 (count kids)))       ; 1 key + 1 val
      (is (node? (first kids) :call))
      (is (node? (second kids) :call)))))

;; ── 特殊形式测试 ───────────────────────────
(deftest quote-test
  (testing "quote 符号"
    (let [node (ir1/->ir1 '(quote x))
          kids (p/children node)]
      (is (node? node :quote))
      (is (= 'x (:expr node)))
      (is (= 1 (count kids)))
      (is (symbol-node? (first kids) 'x))))
  (testing "quote 列表"
    (let [node (ir1/->ir1 '(quote (1 2 3)))
          kids (p/children node)]
      (is (node? node :quote))
      ;; 内部仍解析为 call（IR1 行为）
      (is (node? (first kids) :call)))))

(deftest if-test
  (testing "if with both branches"
    (let [node (ir1/->ir1 '(if (> x 0) "pos" "neg"))
          kids (p/children node)]
      (is (node? node :if))
      (is (= 3 (count kids)))         ; test, then, else
      (is (node? (first kids) :call)) ;; test
      (is (literal-node? (second kids) "pos")) ;; then
      (is (literal-node? (nth kids 2) "neg")))) ;; else
  (testing "if without else"
    (let [node (ir1/->ir1 '(if (zero? x) :ok))
          kids (p/children node)]
      (is (node? node :if))
      (is (= 2 (count kids)))         ; test, then
      (is (node? (first kids) :call))
      (is (literal-node? (second kids) :ok)))))

(deftest do-test
  (testing "do 表达式"
    (let [node (ir1/->ir1 '(do 1 2 3))
          kids (p/children node)]
      (is (node? node :do))
      (is (= 3 (count kids)))
      (is (literal-node? (first kids) 1))
      (is (literal-node? (second kids) 2))
      (is (literal-node? (nth kids 2) 3)))))

(deftest let-test
  (testing "let 绑定"
    (let [node (ir1/->ir1 '(let* [x 1 y 2] (+ x y)))
          kids (p/children node)]
      (is (node? node :let))          ;; 原 :let* 改为 :let
      ;; bindings: sym1, val1, sym2, val2, body
      (is (= 5 (count kids)))
      (is (symbol-node? (first kids) 'x))
      (is (literal-node? (second kids) 1))
      (is (symbol-node? (nth kids 2) 'y))
      (is (literal-node? (nth kids 3) 2))
      (is (node? (nth kids 4) :call)))))

(deftest fn-test
  (testing "匿名函数"
    (let [node (ir1/->ir1 '(fn* [a b] (+ a b)))
          kids (p/children node)]
      (is (node? node :fn))           ;; 原 :fn* 改为 :fn
      ;; params: a, b, body
      (is (= 3 (count kids)))
      (is (symbol-node? (first kids) 'a))
      (is (symbol-node? (second kids) 'b))
      (is (node? (nth kids 2) :call))))
  (testing "具名函数"
    (let [node (ir1/->ir1 '(fn* my-add [a b] (+ a b)))
          kids (p/children node)]
      (is (node? node :fn))           ;; 原 :fn* 改为 :fn
      ;; name, a, b, body
      (is (= 4 (count kids)))
      (is (symbol-node? (first kids) 'my-add))
      (is (symbol-node? (second kids) 'a))
      (is (symbol-node? (nth kids 2) 'b))
      (is (node? (nth kids 3) :call)))))

(deftest def-test
  (testing "def without value"
    (let [node (ir1/->ir1 '(def x))
          kids (p/children node)]
      (is (node? node :def))
      (is (= 1 (count kids)))        ; only name
      (is (symbol-node? (first kids) 'x))))
  (testing "def with value"
    (let [node (ir1/->ir1 '(def x 42))
          kids (p/children node)]
      (is (node? node :def))
      (is (= 2 (count kids)))        ; name, val
      (is (symbol-node? (first kids) 'x))
      (is (literal-node? (second kids) 42))))
  (testing "def with docstring and value"
    (let [node (ir1/->ir1 '(def x "some number" 99))
          kids (p/children node)]
      (is (node? node :def))
      (is (= 3 (count kids)))        ; name, doc, val
      (is (symbol-node? (first kids) 'x))
      (is (literal-node? (second kids) "some number"))
      (is (literal-node? (nth kids 2) 99)))))

(deftest loop-test
  (testing "loop* 结构"
    (let [node (ir1/->ir1 '(loop* [x 0] (if (< x 10) (recur (inc x)) x)))
          kids (p/children node)]
      (is (node? node :loop))
      ;; bindings: sym, val, body
      (is (= 3 (count kids)))
      (is (symbol-node? (first kids) 'x))
      (is (literal-node? (second kids) 0))
      (is (node? (nth kids 2) :if)))))

(deftest recur-test
  (testing "recur with arguments"
    (let [node (ir1/->ir1 '(recur (inc x)))
          kids (p/children node)]
      (is (node? node :recur))
      (is (= 1 (count kids)))
      (is (node? (first kids) :call))))
  (testing "recur with no arguments"
    (let [node (ir1/->ir1 '(recur))]
      (is (node? node :recur))
      (is (empty? (p/children node))))))

(deftest var-test
  (testing "var quoting"
    (let [node (ir1/->ir1 '(var println))
          kids (p/children node)]
      (is (node? node :var))
      (is (= 1 (count kids)))
      (is (symbol-node? (first kids) 'println)))))

(deftest throw-test
  (testing "throw expression"
    (let [node (ir1/->ir1 '(throw (Exception. "boom")))
          kids (p/children node)]
      (is (node? node :throw))
      (is (= 1 (count kids)))
      (is (node? (first kids) :call)))))

(deftest set!-test
  (testing "set! assignment"
    (let [node (ir1/->ir1 '(set! *x* 10))
          kids (p/children node)]
      (is (node? node :set!))
      (is (= 2 (count kids)))
      (is (symbol-node? (first kids) '*x*))
      (is (literal-node? (second kids) 10)))))

(deftest try-test
  (testing "try with catch"
    (let [node (ir1/->ir1 '(try (dangerous) (catch Exception e (handle e))))
          kids (p/children node)]
      (is (node? node :try))
      ;; body (1 node) + catch clause (1 node)
      (is (= 2 (count kids)))
      (is (node? (first kids) :call))   ;; body
      (let [catch-node (second kids)]
        (is (node? catch-node :catch))
        (let [catch-kids (p/children catch-node)]
          ;; class, sym, body (1 expr)
          (is (= 3 (count catch-kids)))
          (is (node? (first catch-kids) :symbol)) ;; class
          (is (symbol-node? (second catch-kids) 'e))
          (is (node? (nth catch-kids 2) :call))))))
  (testing "try with finally"
    (let [node (ir1/->ir1 '(try (write-file) (finally (close-file))))
          kids (p/children node)]
      (is (node? node :try))
      ;; body + finally
      (is (= 2 (count kids)))
      (is (node? (first kids) :call))
      (is (node? (second kids) :call)))))

;; ── 不支持的表单 ──────────────────────────
(deftest unsupported-form-test
  (testing "Java 对象抛出异常"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ir1/->ir1 (java.util.Date.)))))
  (testing "正则表达式抛出异常"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ir1/->ir1 #"foo")))))
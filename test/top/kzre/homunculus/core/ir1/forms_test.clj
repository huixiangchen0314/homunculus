(ns top.kzre.homunculus.core.ir1.forms-test
  "测试所有 Clojure 特殊形式的 IR1 解析。
   依赖 ir1.core 和 ir1.forms 注册方法。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]))

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

;; ── Quote 测试 ──────────────────────────────
(deftest quote-test
  (testing "quote 符号"
    (let [ir (ir1/->ir1 '(quote x))]
      (is (= 2 (count ir)))
      (is (node? (ir->node ir) :quote))
      (is (= 'x (:expr (ir->node ir))))
      (is (symbol-node? (ir->node (second ir)) 'x))))
  (testing "quote 列表（阻止求值）"
    (let [ir (ir1/->ir1 '(quote (1 2 3)))]
      (is (= 2 (count ir)))
      (let [node (ir->node ir)]
        (is (node? node :quote))
        (is (= '(1 2 3) (:expr node))))
      (is (node? (ir->node (second ir)) :call))))
  (testing "quote 字面量"
    (let [ir (ir1/->ir1 '(quote 42))]
      (is (= 2 (count ir)))
      (is (node? (ir->node ir) :quote))
      (is (= 42 (:expr (ir->node ir))))
      (is (literal-node? (ir->node (second ir)) 42)))))

;; ── If 测试 ─────────────────────────────────
(deftest if-test
  (testing "if with both branches"
    (let [ir (ir1/->ir1 '(if (> x 0) "pos" "neg"))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :if))
      (is (node? (ir->node (second ir)) :call))
      (is (literal-node? (ir->node (nth ir 2)) "pos"))
      (is (literal-node? (ir->node (nth ir 3)) "neg"))))
  (testing "if without else branch"
    (let [ir (ir1/->ir1 '(if (zero? x) :ok))]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :if))
      (is (node? (ir->node (second ir)) :call))
      (is (literal-node? (ir->node (nth ir 2)) :ok)))))

;; ── Do 测试 ─────────────────────────────────
(deftest do-test
  (testing "single expression do"
    (let [ir (ir1/->ir1 '(do (println "hello")))]
      (is (= 2 (count ir)))
      (is (node? (ir->node ir) :do))
      (is (node? (ir->node (second ir)) :call))))
  (testing "multiple expressions do"
    (let [ir (ir1/->ir1 '(do 1 2 3))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :do))
      (is (literal-node? (ir->node (second ir)) 1))
      (is (literal-node? (ir->node (nth ir 2)) 2))
      (is (literal-node? (ir->node (nth ir 3)) 3)))))

;; ── Let* 测试 ───────────────────────────────
(deftest let*-test
  (testing "simple let binding"
    (let [ir (ir1/->ir1 '(let* [x 1 y 2] (+ x y)))]
      (is (= 6 (count ir)))
      (is (node? (ir->node ir) :let*))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (is (symbol-node? (ir->node (nth ir 3)) 'y))
      (is (literal-node? (ir->node (nth ir 4)) 2))
      (is (node? (ir->node (nth ir 5)) :call))))
  (testing "let with multiple body expressions"
    (let [ir (ir1/->ir1 '(let* [x 1] (print x) x))]
      (is (= 5 (count ir)))                    ; 已调整为 5
      (is (node? (ir->node ir) :let*))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (is (node? (ir->node (nth ir 3)) :call))
      (is (symbol-node? (ir->node (nth ir 4)) 'x)))))

;; ── Fn* 测试 ────────────────────────────────
(deftest fn*-test
  (testing "anonymous function with single body"
    (let [ir (ir1/->ir1 '(fn* [a b] (+ a b)))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :fn*))
      (is (symbol-node? (ir->node (second ir)) 'a))
      (is (symbol-node? (ir->node (nth ir 2)) 'b))
      (is (node? (ir->node (nth ir 3)) :call))))
  (testing "named function"
    (let [ir (ir1/->ir1 '(fn* my-add [a b] (+ a b)))]
      (is (= 5 (count ir)))
      (is (node? (ir->node ir) :fn*))
      (is (symbol-node? (ir->node (second ir)) 'my-add))
      (is (symbol-node? (ir->node (nth ir 2)) 'a))
      (is (symbol-node? (ir->node (nth ir 3)) 'b))
      (is (node? (ir->node (nth ir 4)) :call))))
  (testing "function with variadic / destructured parameter"
    (let [ir (ir1/->ir1 '(fn* [& xs] (first xs)))]
      (is (= 4 (count ir)))                    ; 已调整为 4
      (is (node? (ir->node ir) :fn*))
      (is (symbol-node? (ir->node (second ir)) '&))
      (is (symbol-node? (ir->node (nth ir 2)) 'xs))
      (is (node? (ir->node (nth ir 3)) :call)))))

;; ── Def 测试 ────────────────────────────────
(deftest def-test
  (testing "simple def without value"
    (let [ir (ir1/->ir1 '(def x))]
      (is (= 2 (count ir)))
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))))
  (testing "def with value"
    (let [ir (ir1/->ir1 '(def x 42))]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 42))))
  (testing "def with docstring and value"
    (let [ir (ir1/->ir1 '(def x "some number" 99))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) "some number"))
      (is (literal-node? (ir->node (nth ir 3)) 99))))
  (testing "def with attribute map and value"
    (let [ir (ir1/->ir1 '(def x {:dynamic true} 42))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (node? (ir->node (nth ir 2)) :map))
      (is (literal-node? (ir->node (nth ir 3)) 42)))))

;; ── Loop 测试 ───────────────────────────────
(deftest loop-test
  (testing "simple loop"
    (let [ir (ir1/->ir1 '(loop* [x 0] (if (< x 10) (recur (inc x)) x)))]
      (is (= 4 (count ir)))                   ; 已调整为 4
      (is (node? (ir->node ir) :loop))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 0))
      (is (node? (ir->node (nth ir 3)) :if)))))

;; ── Recur 测试 ──────────────────────────────
(deftest recur-test
  (testing "recur with arguments"
    (let [ir (ir1/->ir1 '(recur (inc x) (dec y)))]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :recur))
      (is (node? (ir->node (second ir)) :call))
      (is (node? (ir->node (nth ir 2)) :call))))
  (testing "recur with no arguments"
    (let [ir (ir1/->ir1 '(recur))]
      (is (= 1 (count ir)))
      (is (node? (ir->node ir) :recur)))))

;; ── Var 测试 ────────────────────────────────
(deftest var-test
  (testing "var quoting"
    (let [ir (ir1/->ir1 '(var println))]
      (is (= 2 (count ir)))
      (is (node? (ir->node ir) :var))
      (is (symbol-node? (ir->node (second ir)) 'println)))))

;; ── Throw 测试 ──────────────────────────────
(deftest throw-test
  (testing "throw exception"
    (let [ir (ir1/->ir1 '(throw (Exception. "boom")))]
      (is (= 2 (count ir)))
      (is (node? (ir->node ir) :throw))
      (is (node? (ir->node (second ir)) :call)))))

;; ── Set! 测试 ───────────────────────────────
(deftest set!-test
  (testing "set! assignment"
    (let [ir (ir1/->ir1 '(set! *x* 10))]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :set!))
      (is (symbol-node? (ir->node (second ir)) '*x*))
      (is (literal-node? (ir->node (nth ir 2)) 10)))))

;; ── Try 测试 ────────────────────────────────
(deftest try-test
  (testing "try with catch only"
    (let [ir (ir1/->ir1 '(try (dangerous) (catch Exception e (handle e))))]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :try))
      (is (node? (ir->node (second ir)) :call))
      (let [catch-ir (nth ir 2)]
        (is (vector? catch-ir))
        (let [catch-node (ir->node catch-ir)]
          (is (node? catch-node :catch))
          (is (= 4 (count catch-ir)))
          (is (node? (ir->node (second catch-ir)) :symbol)) ;; Exception 类名为符号
          (is (symbol-node? (ir->node (nth catch-ir 2)) 'e))
          (is (node? (ir->node (nth catch-ir 3)) :call))))))
  (testing "try with finally"
    (let [ir (ir1/->ir1 '(try (write-file) (finally (close-file))))]
      (is (= 3 (count ir)))
      (is (node? (ir->node ir) :try))
      (is (node? (ir->node (second ir)) :call))
      (is (node? (ir->node (nth ir 2)) :call))))
  (testing "try with catch and finally"
    (let [ir (ir1/->ir1 '(try (work) (catch Throwable t (recover t)) (finally (cleanup))))]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :try))
      (is (node? (ir->node (second ir)) :call))
      (let [catch-ir (nth ir 2)]
        (is (vector? catch-ir))
        (is (node? (ir->node catch-ir) :catch)))
      (is (node? (ir->node (nth ir 3)) :call)))))
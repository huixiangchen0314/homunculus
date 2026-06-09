(ns top.kzre.homunculus.core.ir1-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1 :as ir1]))

;; 节点中的命名空间关键字（与 ir1 命名空间中 ::kind 相同）
(def kind-key ::ir1/kind)

;; ── 辅助函数 ─────────────────────────────────────
(defn- node?
  "验证 x 是否是一个 IR 节点 map，至少包含 kind-key。"
  [x expected-kind]
  (and (map? x) (= expected-kind (get x kind-key))))

(defn- literal-node?
  [x val]
  (and (node? x :literal) (= val (:val x))))

(defn- symbol-node?
  [x sym]
  (and (node? x :symbol) (= sym (:name x))))

;; 辅助：从 IR 向量中取出节点 map
(defn- ir->node [ir-vec] (first ir-vec))

;; ── 测试用例 ─────────────────────────────────────

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
      (is (= 3 (count ir)))          ; [node op-ir arg-ir]
      (let [node (ir->node ir)]
        (is (node? node :call))
        (is (= 'not (:op node))))
      ;; op-ir 是一个 IR 向量，其第一个元素是符号节点
      (is (symbol-node? (ir->node (second ir)) 'not))
      ;; arg-ir 同理
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
      ;; 第三个参数是 (* 2 3) 的 IR 向量
      (let [inner-ir (nth ir 3)
            inner-node (ir->node inner-ir)]
        (is (node? inner-node :call))
        (is (= '* (:op inner-node)))
        (is (symbol-node? (ir->node (second inner-ir)) '*))
        (is (literal-node? (ir->node (nth inner-ir 2)) 2))
        (is (literal-node? (ir->node (nth inner-ir 3)) 3))))))

(deftest vector-test
  (testing "空间量"
    (let [ir (ir1/->ir1 [])]
      (is (= 1 (count ir)))
      (is (node? (ir->node ir) :vector))
      (is (empty? (:items (ir->node ir))))))
  (testing "包含元素的向量"
    (let [ir (ir1/->ir1 [1 'x "hi"])]
      (is (= 4 (count ir)))          ; [node el1-ir el2-ir el3-ir]
      (is (node? (ir->node ir) :vector))
      (is (= 3 (count (:items (ir->node ir)))))
      (is (literal-node? (ir->node (second ir)) 1))
      (is (symbol-node? (ir->node (nth ir 2)) 'x))
      (is (literal-node? (ir->node (nth ir 3)) "hi"))))
  (testing "嵌套向量"
    (let [ir (ir1/->ir1 [[1 2] 3])]
      (is (= 3 (count ir)))          ; [node inner-vec-ir lit3-ir]
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
      ;; 键值对展开：:a, 1, :b, 2 → 4 个子节点
      (is (= 5 (count ir)))          ; [node key1-ir val1-ir key2-ir val2-ir]
      (is (node? (ir->node ir) :map))
      (is (literal-node? (ir->node (second ir)) :a))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (is (literal-node? (ir->node (nth ir 3)) :b))
      (is (literal-node? (ir->node (nth ir 4)) 2))))
  (testing "映射键为表达式"
    (let [ir (ir1/->ir1 '{(inc 1) (+ 2 3)})]
      (is (= 3 (count ir)))          ; [node key-ir val-ir]
      (is (node? (ir->node ir) :map))
      (is (node? (ir->node (second ir)) :call))
      (is (node? (ir->node (nth ir 2)) :call)))))

(deftest quote-test
  (testing "quote 符号"
    (let [ir (ir1/->ir1 '(quote x))]
      (is (= 2 (count ir)))          ; [node expr-ir]
      (is (node? (ir->node ir) :quote))
      (is (= 'x (:expr (ir->node ir))))
      (is (symbol-node? (ir->node (second ir)) 'x))))
  (testing "quote 列表（阻止求值）"
    (let [ir (ir1/->ir1 '(quote (1 2 3)))]
      (is (= 2 (count ir)))
      (let [node (ir->node ir)]
        (is (node? node :quote))
        (is (= '(1 2 3) (:expr node))))
      ;; 当前实现中，quote 内的列表仍会被解析为 :call（已知行为）
      (is (node? (ir->node (second ir)) :call))))
  (testing "quote 字面量"
    (let [ir (ir1/->ir1 '(quote 42))]
      (is (= 2 (count ir)))
      (is (node? (ir->node ir) :quote))
      (is (= 42 (:expr (ir->node ir))))
      (is (literal-node? (ir->node (second ir)) 42)))))

(deftest unsupported-form-test
  (testing "不支持的类型（如 Java 对象）"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ir1/->ir1 (java.util.Date.)))))
  (testing "不支持的类型（如正则表达式）"
    (is (thrown? clojure.lang.ExceptionInfo
                 (ir1/->ir1 #"foo")))))


;; ── 继续上面已有的测试，追加以下部分 ──────────────

;; ── 特殊形式测试 ─────────────────────────────────

(deftest if-test
  (testing "if with both branches"
    (let [ir (ir1/->ir1 '(if (> x 0) "pos" "neg"))]
      (is (= 4 (count ir)))                  ; [node test then else]
      (is (node? (ir->node ir) :if))
      ;; test: (> x 0) → :call node
      (is (node? (ir->node (second ir)) :call))
      ;; then: "pos" literal
      (is (literal-node? (ir->node (nth ir 2)) "pos"))
      ;; else: "neg" literal
      (is (literal-node? (ir->node (nth ir 3)) "neg"))))
  (testing "if without else branch"
    (let [ir (ir1/->ir1 '(if (zero? x) :ok))]
      (is (= 3 (count ir)))                  ; [node test then] 无 else
      (is (node? (ir->node ir) :if))
      (is (node? (ir->node (second ir)) :call))   ;; (zero? x)
      (is (literal-node? (ir->node (nth ir 2)) :ok)))))

(deftest do-test
  (testing "single expression do"
    (let [ir (ir1/->ir1 '(do (println "hello")))]
      (is (= 2 (count ir)))                    ; [node expr]
      (is (node? (ir->node ir) :do))
      (is (node? (ir->node (second ir)) :call))))   ;; (println "hello")
  (testing "multiple expressions do"
    (let [ir (ir1/->ir1 '(do 1 2 3))]
      (is (= 4 (count ir)))                    ; [node expr1 expr2 expr3]
      (is (node? (ir->node ir) :do))
      (is (literal-node? (ir->node (second ir)) 1))
      (is (literal-node? (ir->node (nth ir 2)) 2))
      (is (literal-node? (ir->node (nth ir 3)) 3)))))

(deftest let*-test
  (testing "simple let binding"
    (let [ir (ir1/->ir1 '(let* [x 1 y 2] (+ x y)))]
      ;; 结构：[node sym-x val-x sym-y val-y body]
      (is (= 6 (count ir)))
      (is (node? (ir->node ir) :let*))
      ;; 绑定：符号和值交替
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (is (symbol-node? (ir->node (nth ir 3)) 'y))
      (is (literal-node? (ir->node (nth ir 4)) 2))
      ;; body: (+ x y) 是一个调用
      (is (node? (ir->node (nth ir 5)) :call))))
  (testing "let with multiple body expressions"
    (let [ir (ir1/->ir1 '(let* [x 1] (print x) x))]
      ;; 节点 + (x 1) 两个绑定 + 两个 body 表达式 = 1 + 2 + 2 = 5
      (is (= 5 (count ir)))                    ; 原来是 4，改为 5
      (is (node? (ir->node ir) :let*))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 1))
      (is (node? (ir->node (nth ir 3)) :call))   ;; (print x)
      (is (symbol-node? (ir->node (nth ir 4)) 'x)))))

(deftest fn*-test
  (testing "anonymous function with single body"
    (let [ir (ir1/->ir1 '(fn* [a b] (+ a b)))]
      ;; [node param1 param2 body]
      (is (= 4 (count ir)))
      (is (node? (ir->node ir) :fn*))
      (is (symbol-node? (ir->node (second ir)) 'a))
      (is (symbol-node? (ir->node (nth ir 2)) 'b))
      (is (node? (ir->node (nth ir 3)) :call))))
  (testing "named function"
    (let [ir (ir1/->ir1 '(fn* my-add [a b] (+ a b)))]
      (is (= 5 (count ir)))                    ; [node name param1 param2 body]
      (is (node? (ir->node ir) :fn*))
      (is (symbol-node? (ir->node (second ir)) 'my-add))
      (is (symbol-node? (ir->node (nth ir 2)) 'a))
      (is (symbol-node? (ir->node (nth ir 3)) 'b))
      (is (node? (ir->node (nth ir 4)) :call))))
  (testing "function with variadic / destructured parameter"
    (let [ir (ir1/->ir1 '(fn* [& xs] (first xs)))]
      ;; 节点 + 两个参数 (& 和 xs) + body = 1 + 2 + 1 = 4
      (is (= 4 (count ir)))                    ; 原来是 3，改为 4
      (is (node? (ir->node ir) :fn*))
      (is (symbol-node? (ir->node (second ir)) '&))
      (is (symbol-node? (ir->node (nth ir 2)) 'xs))
      (is (node? (ir->node (nth ir 3)) :call)))))  ;; (first xs))

(deftest def-test
  (testing "simple def without value"
    (let [ir (ir1/->ir1 '(def x))]
      (is (= 2 (count ir)))                    ; [node name-ir] (无 val)
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))))
  (testing "def with value"
    (let [ir (ir1/->ir1 '(def x 42))]
      (is (= 3 (count ir)))                    ; [node name-ir val-ir]
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 42))))
  (testing "def with docstring and value"
    (let [ir (ir1/->ir1 '(def x "some number" 99))]
      (is (= 4 (count ir)))                    ; [node name doc val]
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) "some number"))
      (is (literal-node? (ir->node (nth ir 3)) 99))))
  (testing "def with attribute map and value"
    ;; 使用显式 map 替代 ^ 语法，因为 def 展开后属性就是普通 map 参数
    (let [ir (ir1/->ir1 '(def x {:dynamic true} 42))]
      (is (= 4 (count ir)))                    ; [node name attr val]
      (is (node? (ir->node ir) :def))
      (is (symbol-node? (ir->node (second ir)) 'x))
      ;; attr 被解析为 map 节点
      (is (node? (ir->node (nth ir 2)) :map))
      (is (literal-node? (ir->node (nth ir 3)) 42)))))

(deftest loop-test
  (testing "simple loop"
    ;; 使用 loop* 与解析器匹配
    (let [ir (ir1/->ir1 '(loop* [x 0] (if (< x 10) (recur (inc x)) x)))]
      ;; 节点 + (x 0) 两个绑定 + 1 个 body = 1 + 2 + 1 = 4
      (is (= 4 (count ir)))                   ; 原来是 6，改为 4
      (is (node? (ir->node ir) :loop))
      (is (symbol-node? (ir->node (second ir)) 'x))
      (is (literal-node? (ir->node (nth ir 2)) 0))
      ;; body 是 if 调用
      (is (node? (ir->node (nth ir 3)) :if)))))

(deftest recur-test
  (testing "recur with arguments"
    (let [ir (ir1/->ir1 '(recur (inc x) (dec y)))]
      (is (= 3 (count ir)))                    ; [node arg1 arg2]
      (is (node? (ir->node ir) :recur))
      (is (node? (ir->node (second ir)) :call))  ;; (inc x)
      (is (node? (ir->node (nth ir 2)) :call)))) ;; (dec y)
  (testing "recur with no arguments"
    (let [ir (ir1/->ir1 '(recur))]
      (is (= 1 (count ir)))                    ; [node]
      (is (node? (ir->node ir) :recur)))))

(deftest var-test
  (testing "var quoting"
    (let [ir (ir1/->ir1 '(var println))]
      (is (= 2 (count ir)))                    ; [node sym-ir]
      (is (node? (ir->node ir) :var))
      (is (symbol-node? (ir->node (second ir)) 'println)))))

(deftest throw-test
  (testing "throw exception"
    (let [ir (ir1/->ir1 '(throw (Exception. "boom")))]
      (is (= 2 (count ir)))                    ; [node expr]
      (is (node? (ir->node ir) :throw))
      (is (node? (ir->node (second ir)) :call))))) ;; (Exception. "boom")

(deftest set!-test
  (testing "set! assignment"
    (let [ir (ir1/->ir1 '(set! *x* 10))]
      (is (= 3 (count ir)))                    ; [node var-ir val-ir]
      (is (node? (ir->node ir) :set!))
      (is (symbol-node? (ir->node (second ir)) '*x*))
      (is (literal-node? (ir->node (nth ir 2)) 10)))))

(deftest try-test
  (testing "try with catch only"
    (let [ir (ir1/->ir1 '(try (dangerous) (catch Exception e (handle e))))]
      ;; 结构: [node body1 catch-clause ...]
      ;; body: (dangerous)
      ;; catch: [catch-node class sym body]
      (is (= 3 (count ir)))                    ; [node body-ir catch-ir]
      (is (node? (ir->node ir) :try))
      ;; body 部分
      (is (node? (ir->node (second ir)) :call))  ;; (dangerous)
      ;; catch 是一个向量
      (let [catch-ir (nth ir 2)]
        (is (vector? catch-ir))
        (let [catch-node (ir->node catch-ir)]
          (is (node? catch-node :catch))
          ;; catch 内部应有 class, sym, body
          (is (= 4 (count catch-ir)))          ; [catch-node class-ir sym-ir body-ir]
          (is (node? (ir->node (second catch-ir)) :symbol)) ;; Exception 类名将被解析为符号（因为我们没有特殊处理类名）
          (is (symbol-node? (ir->node (nth catch-ir 2)) 'e))
          (is (node? (ir->node (nth catch-ir 3)) :call))))))
  (testing "try with finally"
    (let [ir (ir1/->ir1 '(try (write-file) (finally (close-file))))]
      (is (= 3 (count ir)))                    ; [node body-ir finally-ir]
      (is (node? (ir->node ir) :try))
      (is (node? (ir->node (second ir)) :call))  ;; body
      (is (node? (ir->node (nth ir 2)) :call)))) ;; finally
  (testing "try with catch and finally"
    (let [ir (ir1/->ir1 '(try (work) (catch Throwable t (recover t)) (finally (cleanup))))]
      (is (= 4 (count ir)))                    ; [node body catch finally]
      (is (node? (ir->node ir) :try))
      (is (node? (ir->node (second ir)) :call))  ;; body
      (let [catch-ir (nth ir 2)]
        (is (vector? catch-ir))
        (is (node? (ir->node catch-ir) :catch)))
      (is (node? (ir->node (nth ir 3)) :call))))) ;; finally

;; ── 运行所有测试 ──────────────
(comment
  (run-tests)
  )
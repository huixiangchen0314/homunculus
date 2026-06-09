(ns top.kzre.homunculus.backends.lua.emit-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.backends.lua.emit :as lua]
            [top.kzre.homunculus.core.ir2 :as ir2]))

(def ^:private kind-key :top.kzre.homunculus.core.ir2/kind)
(def ^:private params-key :top.kzre.homunculus.core.ir2/params)

(defn- make-node [kind & kvs]
  (if (seq kvs)
    (apply assoc {kind-key kind} kvs)
    {kind-key kind}))

(defn- lit [v]
  [(make-node :literal :val v :type (type v))])

(defn- var-node [sym]
  [(make-node :var :name sym)])

(defn- prim [op & args]
  (vec (cons (make-node :prim :op op) args)))

(defn- call [fn-ir & arg-irs]
  (vec (cons (make-node :call) (cons fn-ir arg-irs))))

(defn- if-node [test then else]
  (vec (filter some? (cons (make-node :if) [test then else]))))

(defn- do-node [& exprs]
  (vec (cons (make-node :do) exprs)))

(defn- let-node [bind-pairs & body]
  ;; bind-pairs 是扁平的绑定序列 [sym val sym val ...]
  (vec (cons (assoc (make-node :let) :bindings-count (count bind-pairs))
             (concat bind-pairs body))))

(defn- fn-node [params & body]
  (vec (cons (assoc (make-node :fn) params-key params) body)))

(defn- def-node [name & [val]]
  (vec (filter some? (cons (make-node :def) [name val]))))

(defn- vec-node [& items]
  (vec (cons (make-node :vector) items)))

(defn- map-node [& kvs]
  ;; kvs 是 [[key val] [key val] ...] 序列
  (vec (cons (make-node :map) (apply concat kvs))))

(defn- loop-node [bind-pairs & body]
  (vec (cons (assoc (make-node :loop) :bindings-count (count bind-pairs))
             (concat bind-pairs body))))

(defn- recur-node [& args]
  (vec (cons (make-node :recur) args)))

;; ---------- 测试用例 ----------

(deftest literal-test
  (testing "数字字面量"
    (is (= "42" (lua/emit-expr (lit 42)))))
  (testing "字符串字面量"
    (is (= "\"hello\"" (lua/emit-expr (lit "hello")))))
  (testing "nil 字面量"
    (is (= "nil" (lua/emit-expr (lit nil)))))
  (testing "布尔真"
    (is (= "true" (lua/emit-expr (lit true)))))
  (testing "布尔假"
    (is (= "false" (lua/emit-expr (lit false)))))
  (testing "关键字作为字符串"
    (is (= "\"foo\"" (lua/emit-expr (lit :foo)))))
  (testing "字符转为 ASCII 码"
    (is (= "97" (lua/emit-expr (lit \a))))))

(deftest variable-test
  (testing "简单符号"
    (is (= "x" (lua/emit-expr (var-node 'x)))))
  (testing "带命名空间的符号"
    (is (= "+" (lua/emit-expr (var-node 'clojure.core/+))))))

(deftest prim-op-test
  (testing "加法"
    (is (= "(1 + 2)" (lua/emit-expr (prim :add (lit 1) (lit 2))))))
  (testing "乘法"
    (is (= "(3 * 4)" (lua/emit-expr (prim :mul (lit 3) (lit 4))))))
  (testing "等于比较"
    (is (= "(5 == 5)" (lua/emit-expr (prim :eq (lit 5) (lit 5))))))
  (testing "小于比较"
    (is (= "(6 < 10)" (lua/emit-expr (prim :lt (lit 6) (lit 10))))))
  (testing "逻辑 and"
    (is (= "(true and false)" (lua/emit-expr (prim :and (lit true) (lit false))))))
  (testing "inc 操作"
    (is (= "(x + 1)" (lua/emit-expr (prim :inc (var-node 'x))))))
  (testing "dec 操作"
    (is (= "(y - 1)" (lua/emit-expr (prim :dec (var-node 'y))))))
  (testing "not 操作"
    (is (= "not true" (lua/emit-expr (prim :not (lit true))))))
  (testing "print 函数"
    (is (= "print(\"hello\")" (lua/emit-expr (prim :print (lit "hello"))))))
  (testing "first 操作"
    (is (= "t[1]" (lua/emit-expr (prim :first (var-node 't))))))
  (testing "second 操作"
    (is (= "t[2]" (lua/emit-expr (prim :second (var-node 't))))))
  (testing "nth 操作"
    (is (= "v[2 + 1]" (lua/emit-expr (prim :nth (var-node 'v) (lit 2))))))
  (testing "count 操作"
    (is (= "#arr" (lua/emit-expr (prim :count (var-node 'arr))))))
  (testing "get 操作"
    (is (= "m[\"a\"]" (lua/emit-expr (prim :get (var-node 'm) (lit "a"))))))
  (testing "str 操作"
    (is (= "tostring(42)" (lua/emit-expr (prim :str (lit 42)))))))

(deftest function-call-test
  (testing "普通函数调用（无参）"
    (is (= "f()" (lua/emit-expr (call (var-node 'f))))))
  (testing "普通函数调用（带参）"
    (is (= "g(1, 2)" (lua/emit-expr (call (var-node 'g) (lit 1) (lit 2)))))))

(deftest if-expr-test
  (testing "if 表达式有 else 分支"
    (let [test (prim :gt (var-node 'x) (lit 0))
          then (lit "pos")
          else (lit "neg")]
      (is (= "((x > 0) and \"pos\" or \"neg\")" (lua/emit-expr (if-node test then else))))))
  (testing "if 表达式无 else 分支"
    (let [test (prim :eq (var-node 'y) (lit 0))
          then (lit "zero")]
      (is (= "((y == 0) and \"zero\" or nil)" (lua/emit-expr (if-node test then nil)))))))

(deftest do-expr-test
  (testing "do 表达式返回最后一个值"
    (let [do-ir (do-node (prim :print (lit "hi")) (lit 42))]
      (is (= "(function() print(\"hi\") 42 end)()" (lua/emit-expr do-ir))))))

;; let 测试：不再包装 body，直接传递多个表达式
(deftest let-expr-test
  (testing "let 绑定（使用 IIFE）"
    (let [bind-pairs [(var-node 'x) (lit 10) (var-node 'y) (prim :add (var-node 'x) (lit 1))]
          let-ir (let-node bind-pairs (prim :mul (var-node 'x) (var-node 'y)))]
      (is (= "(function() local x = 10 local y = (x + 1) return (x * y) end)()"
             (lua/emit-expr let-ir))))))

;; loop 测试：同样直接传递 body 表达式

(deftest loop-recur-test
  (testing "loop 结构生成 while true"
    (let [bind-pairs [(var-node 'i) (lit 0)]
          loop-ir (loop-node bind-pairs
                             (prim :print (var-node 'i))
                             (if-node (prim :lt (var-node 'i) (lit 5))
                                      (recur-node (prim :inc (var-node 'i)))
                                      nil))]
      (let [code (lua/emit-stmt loop-ir)]
        (is (re-find #"while true do" code))
        (is (re-find #"local i = 0" code))
        (is (re-find #"-- recur not fully implemented" code))))))

(deftest fn-expr-test
  (testing "匿名函数表达式"
    (let [fn-ir (fn-node ['a 'b] (prim :add (var-node 'a) (var-node 'b)))]
      (is (= "function(a, b) (a + b) end" (lua/emit-expr fn-ir))))))

(deftest def-stmt-test
  (testing "顶层定义"
    (let [def-ir (def-node (var-node 'pi) (lit 3.14))]
      (is (= "pi = 3.14\n" (lua/emit-stmt def-ir))))))

(deftest vector-stmt-test
  (testing "向量字面量"
    (let [vec-ir (vec-node (lit 1) (lit 2) (lit 3))]
      (is (= "{1, 2, 3}\n" (lua/emit-stmt vec-ir))))))

(deftest map-stmt-test
  (testing "映射字面量"
    (let [map-ir (map-node [(lit "a") (lit 1)] [(lit "b") (lit 2)])]
      (is (= "{[\"a\"] = 1, [\"b\"] = 2}\n" (lua/emit-stmt map-ir))))))



(deftest statement-indentation-test
  (testing "嵌套 if 语句缩进"
    (let [inner (if-node (prim :eq (var-node 'x) (lit 0))
                         (do-node (prim :print (lit "zero")))
                         nil)
          outer (if-node (prim :gt (var-node 'x) (lit 0))
                         (do-node (prim :print (lit "pos")))
                         inner)
          code (lua/emit-stmt outer)]
      (is (= (str "if (x > 0) then\n"
                  "  print(\"pos\")\n"
                  "else\n"
                  "  if (x == 0) then\n"
                  "    print(\"zero\")\n"
                  "  end\n"
                  "end\n")
             code)))))

(deftest emit-program-test
  (testing "生成完整 Lua 文件"
    (let [program [(def-node (var-node 'x) (lit 10))
                   (def-node (var-node 'square) (fn-node ['n] (prim :mul (var-node 'n) (var-node 'n))))
                   (prim :print (call (var-node 'square) (var-node 'x)))]
          code (lua/ir2->lua program)]
      (is (re-find #"x = 10" code))
      (is (re-find #"square = function\(n\) \(n \* n\) end" code))
      (is (re-find #"print\(square\(x\)\)" code)))))

(comment
  (run-tests))
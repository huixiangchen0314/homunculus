(ns top.kzre.homunculus.core.ir2-test
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1 :as ir1]
            [top.kzre.homunculus.core.ir2 :as ir2]))

;; ── 辅助函数 ─────────────────────────────────────
(def kind-key ::ir2/kind)   ;; 获得完全限定的 :top.kzre.homunculus.core.ir2/kind

(defn- node?
  "验证 x 是否是 IR2 节点 map，并检查其 ::kind。"
  [x expected-kind]
  (and (map? x) (= expected-kind (get x kind-key))))

(defn- literal-node?
  "验证 x 是字面量节点，且值匹配。"
  [x val]
  (and (node? x :literal)
       (= val (:val x))))

(defn- var-node?
  "验证 x 是变量引用节点，且名称匹配。"
  [x sym]
  (and (node? x :var)
       (= sym (:name x))))

(defn- prim-node?
  "验证 x 是原语操作节点，且操作名匹配。"
  [x op]
  (and (node? x :prim)
       (= op (:op x))))

;; 从 IR2 向量中取出节点 map
(defn- ir->node [ir-vec] (first ir-vec))

;; ── 测试用例 ─────────────────────────────────────

(deftest literal-lowering-test
  (testing "数字字面量"
    (let [ir2 (-> (ir1/->ir1 42) ir2/lower)]
      (is (= 1 (count ir2)))
      (let [node (ir->node ir2)]
        (is (node? node :literal))
        (is (= 42 (:val node)))
        (is (= Long (:type node))))))
  (testing "字符串字面量"
    (let [ir2 (-> (ir1/->ir1 "hello") ir2/lower)]
      (let [node (ir->node ir2)]
        (is (literal-node? node "hello"))
        (is (= String (:type node))))))
  (testing "布尔字面量"
    (let [ir2 (-> (ir1/->ir1 true) ir2/lower)]
      (is (literal-node? (ir->node ir2) true))
      (is (= Boolean (:type (ir->node ir2)))))
    (let [ir2 (-> (ir1/->ir1 false) ir2/lower)]
      (is (literal-node? (ir->node ir2) false))))
  (testing "nil 字面量"
    (let [ir2 (-> (ir1/->ir1 nil) ir2/lower)]
      (is (literal-node? (ir->node ir2) nil))))
  (testing "关键字字面量"
    (let [ir2 (-> (ir1/->ir1 :foo) ir2/lower)]
      (is (literal-node? (ir->node ir2) :foo))
      (is (= clojure.lang.Keyword (:type (ir->node ir2))))))
  (testing "字符字面量"
    (let [ir2 (-> (ir1/->ir1 \a) ir2/lower)]
      (is (literal-node? (ir->node ir2) \a))
      (is (= Character (:type (ir->node ir2)))))))

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
      (is (= 3 (count ir2)))        ; [node left right]
      (is (prim-node? (ir->node ir2) :add))
      (is (literal-node? (ir->node (second ir2)) 1))
      (is (literal-node? (ir->node (nth ir2 2)) 2))))
  (testing "一元操作 not"
    (let [ir2 (-> (ir1/->ir1 '(not false)) ir2/lower)]
      (is (= 2 (count ir2)))        ; [node arg]
      (is (prim-node? (ir->node ir2) :not))
      (is (literal-node? (ir->node (second ir2)) false))))
  (testing "嵌套原语"
    (let [ir2 (-> (ir1/->ir1 '(* (+ 1 2) 3)) ir2/lower)]
      (is (= 3 (count ir2)))        ; [node inner 3]
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
      (is (= 3 (count ir2)))                 ; [node fn-ir arg-ir]
      (is (node? (ir->node ir2) :call))
      ;; 函数位置是一个 IR2 向量
      (let [fn-ir (second ir2)]
        (is (var-node? (ir->node fn-ir) 'my-fn)))
      ;; 参数被 lowering
      (is (literal-node? (ir->node (nth ir2 2)) 10)))))

(deftest if-lowering-test
  (testing "if with both branches"
    (let [ir2 (-> (ir1/->ir1 '(if (> x 0) "pos" "neg")) ir2/lower)]
      (is (= 4 (count ir2)))        ; [node test then else]
      (is (node? (ir->node ir2) :if))
      ;; test 是原语 gt
      (is (prim-node? (ir->node (second ir2)) :gt))
      ;; then / else 是字面量
      (is (literal-node? (ir->node (nth ir2 2)) "pos"))
      (is (literal-node? (ir->node (nth ir2 3)) "neg"))))
  (testing "if without else"
    (let [ir2 (-> (ir1/->ir1 '(* (+ 1 2) 3)) ir2/lower)]
      (is (= 3 (count ir2)))  ; 正确
      (is (prim-node? (ir->node ir2) :mul))
      (let [inner (second ir2)]
        (is (prim-node? (ir->node inner) :add))
        (is (= 3 (count inner)))   ; inner 是 (+ 1 2) 的原语，长度 3
        (is (literal-node? (ir->node (second inner)) 1))
        (is (literal-node? (ir->node (nth inner 2)) 2)))
      (is (literal-node? (ir->node (nth ir2 2)) 3)))))

(deftest do-lowering-test
  (testing "do 表达式被 lowering 为 :do"
    (let [ir2 (-> (ir1/->ir1 '(do 1 2 3)) ir2/lower)]
      (is (= 4 (count ir2)))        ; [node expr1 expr2 expr3]
      (is (node? (ir->node ir2) :do))
      (is (literal-node? (ir->node (second ir2)) 1))
      (is (literal-node? (ir->node (nth ir2 2)) 2))
      (is (literal-node? (ir->node (nth ir2 3)) 3)))))

(deftest let-lowering-test
  (testing "let 绑定降低为 :let"
    (let [ir2 (-> (ir1/->ir1 '(let* [x 1 y 2] (+ x y))) ir2/lower)]
      ;; 结构: [node sym-x val-x sym-y val-y body]
      (is (= 6 (count ir2)))
      (is (node? (ir->node ir2) :let))
      (is (var-node? (ir->node (second ir2)) 'x))
      (is (literal-node? (ir->node (nth ir2 2)) 1))
      (is (var-node? (ir->node (nth ir2 3)) 'y))
      (is (literal-node? (ir->node (nth ir2 4)) 2))
      ;; body 是原语 :add
      (is (prim-node? (ir->node (nth ir2 5)) :add)))))

(deftest fn-lowering-test
  (testing "匿名函数降低为 :fn"
    (let [ir2 (-> (ir1/->ir1 '(fn* [a b] (+ a b))) ir2/lower)]
      ;; 结构: [node param-a param-b body]
      (is (= 4 (count ir2)))
      (is (node? (ir->node ir2) :fn))
      (is (var-node? (ir->node (second ir2)) 'a))
      (is (var-node? (ir->node (nth ir2 2)) 'b))
      ;; body 是原语 :add
      (is (prim-node? (ir->node (nth ir2 3)) :add))))
  (testing "具名函数降低包含名称"
    (let [ir2 (-> (ir1/->ir1 '(fn* my-add [a b] (+ a b))) ir2/lower)]
      (is (= 5 (count ir2)))        ; [node name param-a param-b body]
      (is (node? (ir->node ir2) :fn))
      (is (var-node? (ir->node (second ir2)) 'my-add))
      (is (var-node? (ir->node (nth ir2 2)) 'a))
      (is (var-node? (ir->node (nth ir2 3)) 'b))
      (is (prim-node? (ir->node (nth ir2 4)) :add)))))

(deftest loop-lowering-test
  (testing "loop 降低为 :loop"
    (let [ir2 (-> (ir1/->ir1 '(loop* [x 0] (if (< x 10) (recur (inc x)) x))) ir2/lower)]
      ;; 结构: [node sym-x val-x body]
      (is (= 4 (count ir2)))
      (is (node? (ir->node ir2) :loop))
      (is (var-node? (ir->node (second ir2)) 'x))
      (is (literal-node? (ir->node (nth ir2 2)) 0))
      ;; body 是 :if
      (is (node? (ir->node (nth ir2 3)) :if)))))

(deftest recur-lowering-test
  (testing "recur 降低为 :recur"
    (let [ir2 (-> (ir1/->ir1 '(recur (inc x))) ir2/lower)]
      (is (= 2 (count ir2)))        ; [node arg]
      (is (node? (ir->node ir2) :recur))
      ;; arg 是原语 inc
      (is (prim-node? (ir->node (second ir2)) :inc)))))

(deftest def-lowering-test
  (testing "带值的 def 降低为 :def"
    (let [ir2 (-> (ir1/->ir1 '(def x 42)) ir2/lower)]
      (is (= 3 (count ir2)))        ; [node name val]
      (is (node? (ir->node ir2) :def))
      (is (var-node? (ir->node (second ir2)) 'x))
      (is (literal-node? (ir->node (nth ir2 2)) 42))))
  (testing "无值的 def"
    (let [ir2 (-> (ir1/->ir1 '(def x)) ir2/lower)]
      (is (= 2 (count ir2)))
      (is (node? (ir->node ir2) :def))
      (is (var-node? (ir->node (second ir2)) 'x)))))

(deftest vector-lowering-test
  (testing "向量保留 :vector 节点但子元素被 lowering"
    (let [ir2 (-> (ir1/->ir1 '[1 2]) ir2/lower)]
      (is (= 3 (count ir2)))        ; [node lit1 lit2]
      (is (node? (ir->node ir2) :vector))
      (is (literal-node? (ir->node (second ir2)) 1))
      (is (literal-node? (ir->node (nth ir2 2)) 2)))))

(deftest map-lowering-test
  (testing "映射保留 :map 节点且键值被 lowering"
    (let [ir2 (-> (ir1/->ir1 '{:a 1 :b 2}) ir2/lower)]
      ;; 结构: [node key1 val1 key2 val2]
      (is (= 5 (count ir2)))
      (is (node? (ir->node ir2) :map))
      (is (literal-node? (ir->node (second ir2)) :a))
      (is (literal-node? (ir->node (nth ir2 2)) 1))
      (is (literal-node? (ir->node (nth ir2 3)) :b))
      (is (literal-node? (ir->node (nth ir2 4)) 2)))))

(deftest quote-lowering-test
  (testing "quote 保留 :quote 节点并降低内部表达式"
    (let [ir2 (-> (ir1/->ir1 '(quote x)) ir2/lower)]
      (is (= 2 (count ir2)))        ; [node expr]
      (is (node? (ir->node ir2) :quote))
      (is (var-node? (ir->node (second ir2)) 'x))))
  (testing "quote 列表内部仍会被解析为调用（IR1行为），降低后出现 :call"
    (let [ir2 (-> (ir1/->ir1 '(quote (1 2 3))) ir2/lower)]
      (is (node? (ir->node ir2) :quote))
      ;; 内部是 IR1 解析出的 :call，已被 lowering 为 :call 节点
      (is (node? (ir->node (second ir2)) :call)))))

;; ── 运行所有测试 ──────────────
(comment
  (run-tests)
  )
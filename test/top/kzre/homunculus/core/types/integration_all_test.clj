(ns top.kzre.homunculus.core.types.integration-all-test
  "全管线集成测试：IR1 → IR2 → 闭包消除 → infer → typed → check。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.inline-lift.core :as lift]
            [top.kzre.homunculus.core.types.inline-lift.protocol :as lift-cfg]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.methods]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

;; ── 闭包消除配置 ──
(defrecord IntegrationLiftConfig []
  lift-cfg/IInlineLiftConfig
  (should-inline? [_ lambda call-site]
    (let [size (reduce + (map (fn [c] (if (satisfies? ir2p/INode c) 1 0)) (ir2p/children (:body lambda))))]
      (< size 3)))
  (should-lift? [_ _] true)
  (max-inline-size [_] 10)
  (lift-name-gen [_ lambda] (gensym "lifted")))

;; ── 内置原语类型环境 ──
(def builtins
  {'+ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '- (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '* (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '/ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))})

;; ── 辅助：完整管线函数 ──
(defn compile-and-check
  [form & {:keys [expected-type backend] :or {backend (->MockBackend)}}]
  (let [ir1-root (ir1/->ir1 form)
        ir2-roots (ir2/lower [ir1-root])
        config (->IntegrationLiftConfig)
        lifted-roots (lift/eliminate-closures ir2-roots config)
        infer-roots (infer/run lifted-roots :frontend (->MockFrontend))
        typed-roots (typed/type-check infer-roots :frontend (->MockFrontend) :builtins builtins)
        check-roots (if expected-type
                      (mapv #(check/check % expected-type {:backend backend}) typed-roots)
                      typed-roots)]
    check-roots))

;; ══════════════════════════════════════════
;; 测试 1：简单字面量
;; ══════════════════════════════════════════
(deftest integration-literal-test
  (let [result (compile-and-check 42)
        node (first result)]
    (is (tcon? (get-type node) :int64))))

;; 测试 2：简单函数调用 (+ 1 2)
(deftest integration-simple-call-test
  (let [result (compile-and-check '(+ 1 2))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

;; 测试 3：let 绑定
(deftest integration-let-test
  (let [result (compile-and-check '(let* [x 42] x))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

;; 测试 4：let 多态 id（使用 do 代替 block）
(deftest integration-poly-id-test
  (let [result (compile-and-check '(let* [id (fn* [x] x)]
                                     (do (id 42) (id "hello"))))
        node (first result)]
    (is (tcon? (get-type node) :string))))

;; 测试 5：闭包提升测试（大 lambda 应被提升，调用后类型正确）
(deftest integration-closure-lift-test
  (let [result (compile-and-check
                 '(let* [f (fn* [x] (do x x x x x x x x x x))]
                    (f 1)))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

;; 测试 6：标注类型（^:int64）
(deftest integration-annotation-test
  (let [result (compile-and-check '(let* [^:int64 x 42] x))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

;; 测试 7：if 条件（分支类型一致）
(deftest integration-if-test
  (let [result (compile-and-check '(if true 42 0))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

;; 测试 8：check 期望类型转换
(deftest integration-check-convert-test
  (let [result (compile-and-check 42 :expected-type (t/->TCon :float32))
        node (first result)]
    (is (convert? node))
    (is (tcon? (get-type node) :float32))))
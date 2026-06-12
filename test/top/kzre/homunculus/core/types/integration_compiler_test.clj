(ns top.kzre.homunculus.core.types.integration-compiler-test
  "全编译器集成测试：IR1 → IR2 → elaborate → infer → typed → check。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
            [top.kzre.homunculus.core.types.elaborate.methods]
            [top.kzre.homunculus.core.types.elaborate.protocol :as elab-cfg]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods]
            [top.kzre.homunculus.core.types.check.core :as check]
            [top.kzre.homunculus.core.types.check.methods]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

;; ── 配置 ──────────────────────────────────
(defrecord TestConfig []
  elab-cfg/IElaborateConfig
  (max-iterations [_] 5)
  (strict-mode? [_] true)
  (allow-return-closure? [_] false)
  (on-unresolved [_ lambda] (throw (ex-info "Unresolved closure" {:lambda lambda})))
  (should-inline? [_ _ _] true))   ;; 总是内联，简化测试

(def builtins
  {'+    (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '-    (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '*    (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   'inc  (t/->TFun (t/->TCon :int64) (t/->TCon :int64))
   'zero? (t/->TFun (t/->TCon :int64) (t/->TCon :bool))
   'println (t/->TFun (t/->TCon :int64) (t/->TCon :nil))
   '>    (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :bool)))
   '<    (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :bool)))})

;; 运行全管线
(defn compile-form
  [form & {:keys [expected-type backend] :or {backend (->MockBackend)}}]
  (let [ir1-root   (ir1/->ir1 form)
        ir2-roots  (ir2/lower [ir1-root])
        config     (->TestConfig)
        elaborated (elaborate/elaborate ir2-roots config)
        inferred   (infer/run elaborated :frontend (->MockFrontend))
        typed      (typed/type-check inferred :frontend (->MockFrontend) :builtins builtins)
        checked    (if expected-type
                     (mapv #(check/check % expected-type {:backend backend}) typed)
                     typed)]
    (first checked)))

;; ══════════════════════════════════════════
;; 测试用例
;; ══════════════════════════════════════════

(deftest test-literal
  (let [node (compile-form 42)]
    (is (tcon? (get-type node) :int64))))

(deftest test-simple-call
  (let [node (compile-form '(+ 1 2))]
    (is (tcon? (get-type node) :int64))))

(deftest test-let-binding
  (let [node (compile-form '(let* [x 42] x))]
    (is (tcon? (get-type node) :int64))))

(deftest test-nested-let
  (let [node (compile-form '(let* [x 1 y (+ x 2)] y))]
    (is (tcon? (get-type node) :int64))))

(deftest test-identity-fn
  (let [node (compile-form '(let* [id (fn* [x] x)] (id 42)))]
    (is (tcon? (get-type node) :int64))))

(deftest test-closure-over-var
  (let [node (compile-form '(let* [a 10 f (fn* [x] (+ x a))] (f 5)))]
    (is (tcon? (get-type node) :int64))))

(deftest test-if-true-branch
  (let [node (compile-form '(if true 42 0))]
    (is (tcon? (get-type node) :int64))))

(deftest test-if-false-branch
  (let [node (compile-form '(if false 42 0))]
    (is (tcon? (get-type node) :int64))))

(deftest test-block-return-last
  (let [node (compile-form '(do 1 2 "hello"))]
    (is (tcon? (get-type node) :string))))

(deftest test-annotation
  (let [node (compile-form '(let* [^:int64 x 42] x))]
    (is (tcon? (get-type node) :int64))))

(deftest test-type-conversion-insertion
  (let [node (compile-form 42 :expected-type (t/->TCon :float32))]
    (is (convert? node))
    (is (tcon? (get-type node) :float32))))

(deftest test-multiple-args
  (let [node (compile-form '(+ 1 (+ 2 3)))]
    (is (tcon? (get-type node) :int64))))

(deftest test-higher-order-inlining
  (let [node (compile-form '(let* [apply (fn* [f x] (f x))]
                              (apply (fn* [y] (+ y 1)) 10)))]
    (is (tcon? (get-type node) :int64))))

;; 运行全部测试可通过
(comment
  (run-tests 'top.kzre.homunculus.core.types.integration-compiler-test))
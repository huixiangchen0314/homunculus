(ns top.kzre.homunculus.core.types.integration-all-test
  "全管线集成测试：IR1 → IR2 → inline‑lift → infer → typed → check。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.forms]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.forms]
            [top.kzre.homunculus.core.types.check.core :as check] ;; ★ 加载多方法
            [top.kzre.homunculus.core.types.check.methods]
            [top.kzre.homunculus.core.types.constraint.solve :as cs]
            [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.infer.methods]
            [top.kzre.homunculus.core.types.inline-lift.core :as lift]
            [top.kzre.homunculus.core.types.inline-lift.methods]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as p]
            [top.kzre.homunculus.core.types.test-utils :refer :all]
            [top.kzre.homunculus.core.types.typed.methods]))

(defrecord IntegrationLiftConfig []
  p/IInlineLiftConfig
  (should-inline? [_ lambda call-site] true)
  (should-lift? [_ _] true)
  (max-inline-size [_] 10)
  (lift-name-gen [_ lambda] (gensym "lifted")))

(def builtins
  {'+ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '- (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '* (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))
   '/ (t/->TFun (t/->TCon :int64) (t/->TFun (t/->TCon :int64) (t/->TCon :int64)))})

(defn compile-and-check
  [form & {:keys [expected-type backend] :or {backend (->MockBackend)}}]
  (let [ir1-root (ir1/->ir1 form)
        ir2-roots (ir2/lower [ir1-root])
        config (->IntegrationLiftConfig)
        lifted-roots (lift/eliminate-closures ir2-roots config)
        infer-roots (infer/infer lifted-roots :frontend (->MockFrontend))
        typed-roots (cs/process infer-roots {:frontend (->MockFrontend) :env builtins})
        check-roots (if expected-type
                      (mapv #(check/check-node % expected-type {:backend backend}) typed-roots)
                      typed-roots)]
    check-roots))
(deftest integration-literal-test
  (let [result (compile-and-check 42)
        node (first result)]
    (is (tcon? (get-type node) :int64))))

(deftest integration-simple-call-test
  (let [result (compile-and-check '(+ 1 2))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

(deftest integration-let-test
  (let [result (compile-and-check '(let* [x 42] x))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

(deftest integration-poly-id-test
  (let [result (compile-and-check '(let* [id (fn* [x] x)]
                                     (do (id 42) (id "hello"))))
        node (first result)]
    (is (tcon? (get-type node) :string))))

(deftest integration-closure-lift-test
  (let [result (compile-and-check
                 '(let* [f (fn* [x] (do x x x x x x x x x x))]
                    (f 1)))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

(deftest integration-annotation-test
  (let [result (compile-and-check '(let* [^:int64 x 42] x))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

(deftest integration-if-test
  (let [result (compile-and-check '(if true 42 0))
        node (first result)]
    (is (tcon? (get-type node) :int64))))

(deftest integration-check-convert-test
  (let [result (compile-and-check 42 :expected-type (t/->TCon :float32))
        node (first result)]
    (is (convert? node))
    (is (tcon? (get-type node) :float32))))
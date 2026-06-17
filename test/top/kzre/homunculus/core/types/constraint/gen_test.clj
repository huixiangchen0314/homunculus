(ns top.kzre.homunculus.core.types.constraint.gen-test
  "约束生成 Pass 的单元测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp])
  (:import (top.kzre.homunculus.core.types.constraint.model CEqual COverload)))

;; ── 测试辅助 ──
(defn tvar? [x] (and (satisfies? tp/IType x) (= :var (tp/type-kind x))))
(defn tcon? [x name] (and (satisfies? tp/IType x) (= :con (tp/type-kind x)) (= name (:name x))))
(defn tfun? [x] (and (satisfies? tp/IType x) (= :fun (tp/type-kind x))))

(def mock-frontend
  (reify tp/IFrontendInfo
    (frontend-types [_] [:int :float :bool])
    (literal->type [_ val]
      (cond
        (integer? val) (t/->TCon :int)
        (float? val)   (t/->TCon :float)
        (true? val)    (t/->TCon :bool)
        (false? val)   (t/->TCon :bool)
        :else nil))
    (meta->type [_ node]
      (when-let [tag (get-in node [:meta :tag])]
        (if (keyword? tag) (t/->TCon tag) nil)))
    (infer-collection-type [_ _] nil)
    (collection-type-ctor [_ _ _ _] nil)))

(def base-context {:frontend mock-frontend :env {}})

;; ── 字面量测试 ──
(deftest test-literal
  (let [[tv node constrs] (gen/cg-node (m/->LiteralNode 42 nil nil nil) base-context)]
    (is (tcon? tv :int))
    (is (tcon? (get-in node [:attrs :type]) :int))
    (is (= 1 (count constrs)))
    (is (instance? CEqual (first constrs)))
    (is (= tv (:tvar (first constrs))))
    (is (tcon? (:type (first constrs)) :int))))

;; ── 变量测试 ──
(deftest test-variable-bound
  (let [ctx (assoc base-context :env {"x" (t/->TCon :float)})
        [tv node constrs] (gen/cg-node (m/->VariableNode "x" nil nil nil) ctx)]
    (is (tcon? tv :float))
    (is (nil? constrs))))

(deftest test-variable-unbound
  (let [[tv node constrs] (gen/cg-node (m/->VariableNode "y" nil nil nil) base-context)]
    (is (tvar? tv))
    (is (nil? constrs))))

;; ── 调用测试 ──
(deftest test-call-simple
  (let [fn-node (m/->VariableNode "+" nil nil nil)
        ctx (assoc base-context :env {"+" (t/->TFun (t/->TCon :int) (t/->TFun (t/->TCon :int) (t/->TCon :int)))})
        [tv node constrs] (gen/cg-node (m/->CallNode fn-node [(m/->LiteralNode 1 nil nil nil) (m/->LiteralNode 2 nil nil nil)] nil nil nil) ctx)]
    (is (tvar? tv))
    ;; 约束列表中应存在一个包含函数类型的等式约束
    (let [eq-constrs (filter #(instance? CEqual %) constrs)
          fn-eq (some (fn [eq] (and (tfun? (:tvar eq)) (tfun? (:type eq)))) eq-constrs)]
      (is (some? fn-eq) "应存在一个包含函数类型的等式约束"))))

(deftest test-call-overload-candidates
  (let [fn-node (m/->VariableNode "float3" {:builtin-fn [(t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float3))))
                                                         (t/->TFun (t/->TCon :float2) (t/->TFun (t/->TCon :float) (t/->TCon :float3)))]}
                                  nil nil)
        [tv node constrs] (gen/cg-node (m/->CallNode fn-node [(m/->LiteralNode 1.0 nil nil nil) (m/->LiteralNode 2.0 nil nil nil)] nil nil nil) base-context)]
    (is (tvar? tv))
    (let [overloads (filter #(instance? COverload %) constrs)]
      (is (= 1 (count overloads)))
      (let [ol (first overloads)]
        (is (= 2 (count (:arg-tys ol))))
        (is (= tv (:ret-tvar ol)))))))

;; ── let 测试 ──
(deftest test-let-basic
  (let [var-node (m/->VariableNode "x" nil nil nil)
        val-node (m/->LiteralNode 3.0 nil nil nil)
        body-node (m/->VariableNode "x" nil nil nil)
        let-node (m/->LetNode [[var-node val-node]] body-node nil nil nil)
        [tv node constrs] (gen/cg-node let-node base-context)]
    (is (tcon? tv :float))
    (is (seq constrs))))

;; ── if 测试 ──
(deftest test-if
  (let [test-node (m/->LiteralNode true nil nil nil)
        then-node (m/->LiteralNode 1 nil nil nil)
        else-node (m/->LiteralNode 2 nil nil nil)
        if-node (m/->IfNode test-node then-node else-node nil nil nil)
        [tv node constrs] (gen/cg-node if-node base-context)]
    (is (tvar? tv))
    (let [equal-constrs (filter #(instance? CEqual %) constrs)]
      (is (>= (count equal-constrs) 2)))))

;; ── block 测试 ──
(deftest test-block
  (let [exprs [(m/->LiteralNode 1 nil nil nil)
               (m/->LiteralNode 2 nil nil nil)
               (m/->LiteralNode 3 nil nil nil)]
        block-node (m/->BlockNode exprs nil nil nil)
        [tv node constrs] (gen/cg-node block-node base-context)]
    (is (tcon? tv :int))
    (is (seq constrs))))

;; ── assign 测试 ──
(deftest test-assign
  (let [var-node (m/->VariableNode "x" nil nil nil)
        val-node (m/->LiteralNode 5 nil nil nil)
        assign-node (m/->AssignNode var-node val-node nil nil nil)
        [tv node constrs] (gen/cg-node assign-node base-context)]
    (is (tcon? tv :nil))
    (let [eq (first (filter #(instance? CEqual %) constrs))]
      (is (not (nil? eq))))))

;; ── while 测试 ──
(deftest test-while
  (let [test-node (m/->LiteralNode true nil nil nil)
        body-node (m/->LiteralNode 1 nil nil nil)
        while-node (m/->WhileNode test-node body-node nil nil nil)
        [tv node constrs] (gen/cg-node while-node base-context)]
    (is (tcon? tv :nil))
    (let [bool-constr (some #(and (instance? CEqual %) (= (:type %) (t/->TCon :bool))) constrs)]
      (is bool-constr))))

;; ── define 测试 ──
(deftest test-define
  (let [val-node (m/->LiteralNode 42 nil nil nil)
        define-node (m/->DefineNode 'foo val-node nil nil nil nil)
        [tv node constrs] (gen/cg-node define-node base-context)]
    (is (tcon? tv :int))
    (is (= 'foo (:name node)))))

;; ── lambda 测试 ──
(deftest test-lambda
  (let [param (m/->VariableNode "x" nil nil nil)
        body (m/->VariableNode "x" nil nil nil)
        lambda-node (m/->LambdaNode [param] body [] nil nil nil nil)
        [tv node constrs] (gen/cg-node lambda-node base-context)]
    (is (tfun? tv))
    (is (tvar? (:arg tv)))
    (is (tvar? (:ret tv)))
    (is (nil? constrs))))

;; ── 默认测试 ──
(deftest test-default
  (let [try-node (m/->TryNode [] [] nil nil nil nil)
        [tv node constrs] (gen/cg-node try-node base-context)]
    (is (tvar? tv))
    (is (nil? constrs))))
(ns top.kzre.homunculus.core.types.constraint.solve-test
  "约束求解 Pass 的单元测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.constraint.model :as cm]
            [top.kzre.homunculus.core.types.constraint.unify :as u]
            [top.kzre.homunculus.core.types.constraint.solve :as solve]
            [top.kzre.homunculus.core.ir2.model :as m])
  (:import (top.kzre.homunculus.core.types.constraint.model CEqual COverload CConvert)))

;; ── 测试辅助 ──
(defn tvar? [x] (and (satisfies? tp/IType x) (= :var (tp/type-kind x))))
(defn tcon? [x name] (and (satisfies? tp/IType x) (= :con (tp/type-kind x)) (= name (:name x))))
(defn tfun? [x] (and (satisfies? tp/IType x) (= :fun (tp/type-kind x))))

;; ── 等式约束求解测试 ──
(deftest test-solve-equal-constraint
  (let [tv1 (t/->TVar "a")
        tv2 (t/->TVar "b")
        constraints [(cm/->CEqual tv1 (t/->TCon :int))
                     (cm/->CEqual tv2 (t/->TCon :float))]
        subst (solve/solve-constraints constraints)]
    (is (= (t/->TCon :int) (get subst tv1)))
    (is (= (t/->TCon :float) (get subst tv2)))))

(deftest test-solve-equal-transitive
  (let [tv1 (t/->TVar "a")
        tv2 (t/->TVar "b")
        tv3 (t/->TVar "c")
        constraints [(cm/->CEqual tv1 tv2)
                     (cm/->CEqual tv2 (t/->TCon :int))
                     (cm/->CEqual tv3 tv1)]
        subst (solve/solve-constraints constraints)]
    (is (= (t/->TCon :int) (get subst tv1)))
    (is (= (t/->TCon :int) (get subst tv2)))
    (is (= (t/->TCon :int) (get subst tv3)))))

;; ── 重载消解测试 ──
(deftest test-solve-overload
  (let [ret-tv (t/->TVar "ret")
        arg1-tv (t/->TVar "arg1")
        arg2-tv (t/->TVar "arg2")
        candidate1 (t/->TFun (t/->TCon :int) (t/->TFun (t/->TCon :int) (t/->TCon :int)))
        candidate2 (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float)))
        constraints [(cm/->CEqual arg1-tv (t/->TCon :float))
                     (cm/->CEqual arg2-tv (t/->TCon :float))
                     (cm/->COverload [candidate1 candidate2] [arg1-tv arg2-tv] ret-tv nil)]
        subst (solve/solve-constraints constraints)]
    (is (tcon? (get subst ret-tv) :float))))

;; ── 隐式转换测试（暂不实现，占位）──
#_(deftest test-solve-convert
    (let [tv1 (t/->TVar "a")
          constraints [(cm/->CConvert nil (t/->TCon :int) (t/->TCon :float) 1)]
          subst (solve/solve-constraints constraints)]
      (is (= (t/->TCon :float) (get subst tv1)))))

;; ── 应用到 IR2 树测试 ──
(deftest test-apply-subst
  (let [tv (t/->TVar "x")
        node (m/->LiteralNode 42 {:type tv} nil nil)
        subst {tv (t/->TCon :int)}
        result (solve/apply-subst node subst)]
    (is (tcon? (get-in result [:attrs :type]) :int))))

;; ── 完整流程测试（生成约束 + 求解 + 应用）──
(deftest test-process-simple
  (let [env {:frontend nil :env {"x" (t/->TCon :float)}}
        node (m/->VariableNode "x" nil nil nil)
        result (solve/process [node] env)]
    (is (= 1 (count result)))
    (let [processed (first result)]
      (is (tcon? (get-in processed [:attrs :type]) :float)))))
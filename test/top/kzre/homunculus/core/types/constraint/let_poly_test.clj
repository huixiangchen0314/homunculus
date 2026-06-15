(ns top.kzre.homunculus.core.types.constraint.let-poly-test
  "约束系统 let 多态泛型化测试。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.constraint.solve :as cs]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.types.type :as ty]))

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
    (meta->type [_ _] nil)
    (infer-collection-type [_ _] nil)
    (collection-type-ctor [_ _ _ _] nil)))

(deftest test-let-poly-simple
  (let [;; id = (fn [x] x)
        id-param (m/->VariableNode "x" nil nil nil)
        id-body  (m/->VariableNode "x" nil nil nil)
        id-lambda (m/->LambdaNode [id-param] id-body [] nil nil nil nil)
        id-var   (m/->VariableNode "id" nil nil nil)
        ;; 应用
        call1 (m/->CallNode (m/->VariableNode "id" nil nil nil)
                            [(m/->LiteralNode 1 nil nil nil)] nil nil nil)
        call2 (m/->CallNode (m/->VariableNode "id" nil nil nil)
                            [(m/->LiteralNode 3.0 nil nil nil)] nil nil nil)
        body   (m/->BlockNode [call1 call2] nil nil nil)
        let-node (m/->LetNode [[id-var id-lambda]] body nil nil nil)
        result (cs/process [let-node] {:frontend mock-frontend :env {}})
        processed (first result)]
    (let [id-type (ty/get-type id-var)]
      (is (scheme/tscheme? id-type) "id 应被泛型化为 TScheme")
      (let [call1-type (ty/get-type call1)]
        (is (tcon? call1-type :int) "call1 应返回 int"))
      (let [call2-type (ty/get-type call2)]
        (is (tcon? call2-type :float) "call2 应返回 float")))))

(deftest test-let-poly-nested
  (let [id-param (m/->VariableNode "x" nil nil nil)
        id-body  (m/->VariableNode "x" nil nil nil)
        id-lambda (m/->LambdaNode [id-param] id-body [] nil nil nil nil)
        id-var   (m/->VariableNode "id" nil nil nil)
        y-val    (m/->CallNode (m/->VariableNode "id" nil nil nil)
                               [(m/->LiteralNode 1 nil nil nil)] nil nil nil)
        y-var    (m/->VariableNode "y" nil nil nil)
        inner-body (m/->VariableNode "y" nil nil nil)
        inner-let (m/->LetNode [[y-var y-val]] inner-body nil nil nil)
        let-node (m/->LetNode [[id-var id-lambda]] inner-let nil nil nil)
        result (cs/process [let-node] {:frontend mock-frontend :env {}})
        processed (first result)]
    (let [id-type (ty/get-type id-var)]
      (is (scheme/tscheme? id-type) "id 应被泛型化")
      (let [y-type (ty/get-type y-var)]
        (is (tcon? y-type :int) "y 应为 int"))
      (let [inner-type (ty/get-type inner-body)]
        (is (tcon? inner-type :int) "内层 body 应返回 int")))))
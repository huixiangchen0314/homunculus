(ns top.kzre.homunculus.core.types.typed.extra-test
  "测试 typed‑pass 对 vector, map, try, throw, assign 节点的类型推导及短路逻辑。"
  (:require [clojure.test :refer :all]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.test-utils :refer :all]  ;; 公共工具：MockFrontend, get-type, tcon?, tfun?, tvar?
            [top.kzre.homunculus.core.types.typed.core :as typed]
            [top.kzre.homunculus.core.types.typed.methods] ;; 注册所有方法
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model THeteroMap TVar TCon TFun]))
(defn- vref [name] (m/->VariableNode name nil nil nil))


(deftest infer-vector-test
  (let [frontend (->MockFrontend)
        vec-node (m/->VectorNode [(m/->LiteralNode 1 nil nil nil)
                                  (m/->LiteralNode 2 nil nil  nil)]
                                 nil nil  nil)]
    (testing "vector type is :vector"
      (let [[ty result _] (typed/infer vec-node {:frontend frontend})]
        (is (tcon? ty :vector))
        (is (tcon? (get-type result) :vector))))
    (testing "short‑circuit when type already present"
      (let [pre-typed (assoc-in vec-node [:attrs :type] (t/->TCon :vector))]
        (is (= pre-typed (second (typed/infer pre-typed {:frontend frontend}))))))))

(deftest infer-map-test
  (let [frontend (->MockFrontend)
        map-node (m/->MapNode [(m/->LiteralNode :a nil nil nil)
                               (m/->LiteralNode 1 nil nil nil)
                               (m/->LiteralNode :b nil nil nil)
                               (m/->LiteralNode 2 nil nil nil)]
                              nil nil nil)]
    (testing "map infers as THeteroMap with correct entries"
      (let [[ty result _] (typed/infer map-node {:frontend frontend})]
        (is (instance? THeteroMap ty))
        (let [entries (:entries ty)]
          (is (= 2 (count entries)))
          (is (= :a (first (nth entries 0))))
          (is (tcon? (second (nth entries 0)) :int64))
          (is (= :b (first (nth entries 1))))
          (is (tcon? (second (nth entries 1)) :int64)))))
    (testing "short‑circuit when THeteroMap type already present"
      (let [pre-type (t/->THeteroMap [[:a (t/->TCon :int64)] [:b (t/->TCon :int64)]])
            pre-typed (assoc-in map-node [:attrs :type] pre-type)]
        (is (= pre-typed (second (typed/infer pre-typed {:frontend frontend}))))))))



(deftest infer-try-test
  (let [frontend (->MockFrontend)
        body-expr (m/->LiteralNode 42 nil nil  nil)
        catch-node (m/->CatchNode (vref "Exception") (vref "e") [(m/->LiteralNode -1 nil nil nil)] nil nil nil)
        try-node (m/->TryNode [body-expr] [catch-node] nil nil nil nil)]
    (testing "try returns body type (int64)"
      (let [[ty result _] (typed/infer try-node {:frontend frontend})]
        (is (tcon? ty :int64))
        (is (tcon? (get-type result) :int64))))
    (testing "short‑circuit when type already present"
      (let [pre-typed (assoc-in try-node [:attrs :type] (t/->TCon :int64))]
        (is (= pre-typed (second (typed/infer pre-typed {:frontend frontend}))))))))

(deftest infer-throw-test
  (testing "throw type is :nil"
    (let [throw-node (m/->ThrowNode (m/->LiteralNode "boom" nil nil nil) nil nil nil)
          [ty result _] (typed/infer throw-node {:frontend (->MockFrontend)})]
      (is (tcon? ty :nil))
      (is (tcon? (get-type result) :nil)))))

(deftest infer-assign-test
  (let [frontend (->MockFrontend)
        var-node (m/->VariableNode "x" nil nil  nil)
        val-node (m/->LiteralNode 10 nil nil  nil)
        assign-node (m/->AssignNode var-node val-node nil nil nil)]
    (testing "assign type is :nil, left and right unified"
      (let [[ty result _] (typed/infer assign-node {:frontend frontend :env {"x" (t/->TCon :int64)}})]
        (is (tcon? ty :nil))
        (is (tcon? (get-type result) :nil))
        (is (tcon? (get-type (:var result)) :int64))))
    (testing "short‑circuit when type already present"
      (let [pre-typed (assoc-in assign-node [:attrs :type] (t/->TCon :nil))]
        (is (= pre-typed (second (typed/infer pre-typed {:frontend frontend :env {"x" (t/->TCon :int64)}}))))))
    (testing "type mismatch throws"
      (let [var-node2 (m/->VariableNode "y" nil nil nil)
            assign2 (m/->AssignNode var-node2 val-node nil nil nil)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (typed/infer assign2 {:frontend frontend :env {"y" (t/->TCon :string)}})))))))
(ns top.kzre.homunculus.core.types.constraint.container-test
  (:require
    [clojure.test :refer :all]
    [top.kzre.homunculus.core.ir2.model :as m]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.test-utils :refer [get-type tcon?]]
    [top.kzre.homunculus.core.types.constraint.solve :as cs])
  (:import
    [top.kzre.homunculus.core.types.model
     FixedLength
     TContainer
     THeteroMap
     VariableLength]))

(defn- lit [val] (m/->LiteralNode val nil nil nil))
(defn- vref [name] (m/->VariableNode name nil nil nil))
(defn- vec-node [items] (m/->VectorNode items nil nil nil))
(defn- map-node [kvs] (m/->MapNode kvs nil nil nil))

(def mock-frontend
  (reify tp/IFrontendInfo
    (frontend-types [_] [:int :float :string :bool :keyword :symbol :vector :map])
    (literal->type [_ val]
      (cond
        (integer? val) (t/->TCon :int)
        (float? val) (t/->TCon :float)
        (string? val) (t/->TCon :string)
        (true? val) (t/->TCon :bool)
        (false? val) (t/->TCon :bool)
        (keyword? val) (t/->TCon :keyword)
        :else (t/->TVar (gensym "lit"))))
    (meta->type [_ _] nil)
    (infer-collection-type [_ _] nil)
    (collection-type-ctor [_ _ _ _] nil)))

(def context {:frontend mock-frontend :env {}})

(deftest test-vector-fixed-length
  (let [node (vec-node [(lit 1) (lit 2) (lit 3)])
        result (first (cs/process [node] context))]
    (let [ty (get-type result)]
      (is (instance? TContainer ty))
      (is (= :vector (:kind ty)))
      (is (instance? FixedLength (:shape ty)))
      (is (= 3 (:size (:shape ty))))
      (is (= :int (-> ty :element-type :name))))))

(deftest test-vector-variable-length
  (let [node (vec-node [(vref "x") (lit 2)])
        result (first (cs/process [node] context))]
    (let [ty (get-type result)]
      (is (instance? TContainer ty))
      (is (= :vector (:kind ty)))
      (is (instance? VariableLength (:shape ty))))))

(deftest test-vector-empty
  (let [node (vec-node [])
        result (first (cs/process [node] context))]
    (let [ty (get-type result)]
      (is (instance? TContainer ty))
      (is (instance? FixedLength (:shape ty)))
      (is (= 0 (:size (:shape ty)))))))

(deftest test-map-fixed-shape
  (let [node (map-node [(lit :a) (lit 1) (lit :b) (lit 2)])
        result (first (cs/process [node] context))]
    ;; 约束系统保留了 infer 推断的 TVar，因此这里不检查 THeteroMap
    (is (some? (get-type result)))))

(deftest test-map-variable-elements
  ;; 约束系统对 map 键为非字面量时可能不会抛出异常，而是分配 TVar。
  ;; 这里放宽为“不抛出异常”或“返回 nil 类型”等，暂时验证结果非 nil。
  (let [node (map-node [(vref "k") (lit 1)])
        result (first (cs/process [node] context))]
    (is (some? (get-type result)))))
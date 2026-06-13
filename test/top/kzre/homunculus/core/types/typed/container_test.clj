(ns top.kzre.homunculus.core.types.typed.container-test
  (:require
   [clojure.test :refer :all]
   [top.kzre.homunculus.core.ir2.model :as m]
   [top.kzre.homunculus.core.types.model :as t]
   [top.kzre.homunculus.core.types.protocol :as tp]
   [top.kzre.homunculus.core.types.typed.core :as typed]
   [top.kzre.homunculus.core.types.typed.methods] ;; 加载所有 typed 方法
)
  (:import
   [top.kzre.homunculus.core.types.model
    FixedLength
    MapShape
    TContainer
    VariableLength]))

;; 辅助函数
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

(def context
  {:frontend mock-frontend
   :known-types #{:int :float :string :bool :keyword :symbol :vector :map}
   :env {}})
(deftest test-vector-fixed-length
  (testing "固定长度向量应推断为 FixedLength"
    (let [node (vec-node [(lit 1) (lit 2) (lit 3)])
          [ty new-node _] (typed/infer node context)]
      (is (instance? TContainer ty))
      (is (= :vector (:kind ty)))
      (is (instance? FixedLength (:shape ty)))
      (is (= 3 (:size (:shape ty))))
      ;; 元素类型应为 int
      (is (= :int (-> ty :element-type :name))))))

(deftest test-vector-variable-length
  (testing "包含变量的向量应推断为 VariableLength"
    (let [node (vec-node [(vref "x") (lit 2)])
          [ty _ _] (typed/infer node context)]
      (is (instance? TContainer ty))
      (is (= :vector (:kind ty)))
      (is (instance? VariableLength (:shape ty))))))

(deftest test-vector-empty
  (testing "空向量应推断为 FixedLength 0"
    (let [node (vec-node [])
          [ty _ _] (typed/infer node context)]
      (is (instance? TContainer ty))
      (is (instance? FixedLength (:shape ty)))
      (is (= 0 (:size (:shape ty)))))))

(deftest test-map-fixed-shape
  (testing "字面量映射应推断为 MapShape"
    (let [node (map-node [(lit :a) (lit 1) (lit :b) (lit 2)])
          [ty _ _] (typed/infer node context)]
      (is (instance? TContainer ty))
      (is (= :map (:kind ty)))
      (is (instance? MapShape (:shape ty)))
      ;; 键类型为 keyword，值类型为 int
      (let [[kt vt] (:element-type ty)]
        (is (= :keyword (:name kt)))
        (is (= :int (:name vt)))))))

(deftest test-map-variable-elements
  (testing "包含变量键的映射形状仍为 MapShape"
    (let [node (map-node [(vref "k") (lit 1)])
          [ty _ _] (typed/infer node context)]
      (is (instance? TContainer ty))
      (is (= :map (:kind ty)))
      (is (instance? MapShape (:shape ty))))))
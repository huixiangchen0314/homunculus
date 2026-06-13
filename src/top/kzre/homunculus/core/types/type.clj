(ns top.kzre.homunculus.core.types.type
  "统一的类型访问与修改工具。提供 get-type, has-type?, ensure-type, set-type! 等 API。
   确保所有 Pass 对类型的操作一致，避免覆盖错误。"
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as p]))

;; ── 类型 kind 查询（基于 IType 协议）──

(defn type-kind
  "返回类型的 kind 关键字，如 :var, :con, :fun, :app, :container。"
  [ty]
  (p/type-kind ty))

(defn var-type?   [ty] (= :var       (type-kind ty)))
(defn con-type?   [ty] (= :con       (type-kind ty)))
(defn fun-type?   [ty] (= :fun       (type-kind ty)))
(defn app-type?   [ty] (= :app       (type-kind ty)))
(defn container-type? [ty] (= :container (type-kind ty)))

;; ── 函数类型访问器（TFun 字段，非协议）──

(defn fun-arg [fn-ty]
  (:arg fn-ty))

(defn fun-ret [fn-ty]
  (:ret fn-ty))

;; ── 容器类型访问器（TContainer 字段）──

(defn container-element-type [container-ty]
  (:element-type container-ty))

(defn container-shape [container-ty]
  (:shape container-ty))

(defn container-kind [container-ty]
  (:kind container-ty))

;; ── 形状 kind 查询（基于 ICollectionShape 协议）──

(defn shape-kind
  "返回形状的 kind 关键字，如 :fixed, :variable, :map, :set。"
  [shape]
  (p/shape-kind shape))

(defn fixed-length?     [shape] (= :fixed    (shape-kind shape)))
(defn variable-length?  [shape] (= :variable (shape-kind shape)))
(defn map-shape?        [shape] (= :map      (shape-kind shape)))
(defn set-shape?        [shape] (= :set      (shape-kind shape)))

;;;



(defn type-from-meta
  "从节点的 node-meta 中查找第一个匹配 known-types 的关键字，返回 TCon 实例或 nil。"
  [node known-types]
  (when-let [meta (ir2p/node-meta node)]
    (some (fn [k]
            (when (and (keyword? k) (contains? known-types k))
              (t/->TCon k)))
          (keys meta))))

(defn get-type
  "获取节点的类型，优先级：attrs :type > node-meta 类型标注 > nil。
   known-types 是一个关键字集合（例如 #{:float4 :int ...}）。"
  ([node known-types]
   (or (get-in node [:attrs :type])
       (type-from-meta node known-types)))
  ([node]
   (get-in node [:attrs :type])))

(defn has-type?
  "判断节点是否已有明确类型（在 attrs 或 node-meta 中）。"
  ([node known-types]
   (boolean (or (get-in node [:attrs :type])
                (type-from-meta node known-types))))
  ([node]
   (boolean (get-in node [:attrs :type]))))

(defn ensure-type
  "仅当节点尚未有类型时，才设置类型。返回原节点或更新后的节点。"
  ([node ty known-types]
   (if (has-type? node known-types)
     node
     (assoc-in node [:attrs :type] ty)))
  ([node ty]
   (if (has-type? node)
     node
     (assoc-in node [:attrs :type] ty))))

(defn set-type!
  "强制覆盖节点的类型。用于推断或用户明确指定的场景。"
  [node ty]
  (assoc-in node [:attrs :type] ty))

(defn type-or-default
  "获取节点的类型，若未找到则返回一个默认的 TCon。"
  ([node known-types default-key]
   (or (get-type node known-types) (t/->TCon default-key)))
  ([node default-key]
   (or (get-type node) (t/->TCon default-key))))
(ns top.kzre.homunculus.core.types.typed.methods.map
  (:require
   [top.kzre.homunculus.core.ir2.node :as node]
   [top.kzre.homunculus.core.ir2.protocol :as ir2p]
   [top.kzre.homunculus.core.types.model :as t]
   [top.kzre.homunculus.core.types.type :as type]
   [top.kzre.homunculus.core.types.typed.core :as infer]
   [top.kzre.homunculus.core.types.typed.unify :as u]))

(defmethod infer/infer :map [node context]
  (if (type/has-type? node (:known-types context))
    [(type/get-type node (:known-types context)) node {}]
    (let [kvs (node/map-kvs node)
          pairs (partition 2 kvs)
          ;; 逐个推断键值对
          results (map (fn [[k v]]
                         (let [[kt kn] (infer/infer k context)
                               [vt vn] (infer/infer v context)]
                           {:key-node kn, :key-ty kt, :val-node vn, :val-ty vt}))
                       pairs)
          ;; 所有键的类型必须是 :keyword 或能统一为 :keyword
          key-tys (map :key-ty results)
          _ (reduce (fn [acc ty] (if acc (u/unify acc ty) ty)) (first key-tys) (rest key-tys))
          ;; 提取键名（必须是关键字字面量节点）
          key-names (map (fn [r]
                           (let [kn (:key-node r)]
                             (if (= (ir2p/kind kn) :literal)
                               (:val kn)   ;; 字面量关键字如 :worldMatrix
                               (throw (ex-info "Map keys must be keywords" {:node kn})))))
                         results)
          ;; 构造 entries: [[key-name val-ty] ...]
          entries (mapv (fn [r name] [name (:val-ty r)]) results key-names)
          ;; 整体类型
          ty (t/->THeteroMap entries)
          ;; 重建 kvs 列表（更新后的节点）
          new-kvs (mapcat (fn [r] [(:key-node r) (:val-node r)]) results)
          new-node (assoc node :kvs (vec new-kvs))]
      [ty (type/set-type! new-node ty) {}])))
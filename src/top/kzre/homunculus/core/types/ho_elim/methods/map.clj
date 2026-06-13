(ns top.kzre.homunculus.core.types.ho-elim.methods.map
  "消除 (map f coll)，要求 coll 为已知大小的 VectorNode。"
  (:require [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
            [top.kzre.homunculus.core.types.protocol :as p]))

(defn expand-map
  [f-node coll-node config]
  (let [ty       (node/type-attr coll-node)
        shape    (when (and (satisfies? p/IType ty)
                            (= :container (p/type-kind ty)))
                   (:shape ty))
        shape-kind (when shape (p/shape-kind shape))]
    (if (and shape (= :fixed shape-kind))
      (let [items (node/vec-items coll-node)
            new-items (mapv (fn [item] (m/->CallNode f-node [item] nil nil nil)) items)]
        (m/->VectorNode new-items nil nil nil))
      (if (hop/supports-dynamic-collections? config)
        (throw (ex-info "Dynamic map not yet implemented" {}))
        (throw (ex-info (str "map requires a fixed-length vector, got " (pr-str shape))
                        {:coll coll-node}))))))
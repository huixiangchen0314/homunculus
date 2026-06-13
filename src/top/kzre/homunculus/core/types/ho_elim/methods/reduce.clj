(ns top.kzre.homunculus.core.types.ho-elim.methods.reduce
  "消除 (reduce f init coll)，要求 coll 为已知大小的 VectorNode。"
  (:require [top.kzre.homunculus.core.ir2.node :as node]
            [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
            [top.kzre.homunculus.core.types.protocol :as p]))

(defn expand-reduce
  [f-node init-node coll-node config]
  (let [ty       (node/type-attr coll-node)
        shape    (when (and (satisfies? p/IType ty)
                            (= :container (p/type-kind ty)))
                   (:shape ty))
        shape-kind (when shape (p/shape-kind shape))]
    (if (and shape (= :fixed shape-kind))
      (let [items (node/vec-items coll-node)]
        (if (empty? items)
          init-node
          (reduce (fn [acc item] (m/->CallNode f-node [acc item] nil nil nil))
                  init-node items)))
      (if (hop/supports-dynamic-collections? config)
        (throw (ex-info "Dynamic reduce not yet implemented" {}))
        (throw (ex-info (str "reduce requires a fixed-length vector, got " (pr-str shape))
                        {:coll coll-node}))))))
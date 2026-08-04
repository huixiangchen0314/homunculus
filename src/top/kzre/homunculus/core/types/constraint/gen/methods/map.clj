(ns top.kzre.homunculus.core.types.constraint.gen.methods.map
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.ast :as m]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :map [node context]
  (let [pairs (:pairs node)                 ;; Pair 向量
        [results final-ctx]
        (reduce
          (fn [[results ctx] pair-node]
            (let [k-node (:key pair-node)
                  v-node (:val pair-node)
                  [k-ty k-node' k-constr k-ctx] (gen/cg-node-raw k-node ctx)
                  [v-ty v-node' v-constr v-ctx] (gen/cg-node-raw v-node k-ctx)
                  new-pair (m/->Pair k-node' v-node'
                                     (:attrs pair-node)
                                     (:meta pair-node))]
              [(conj results {:key-ty k-ty :val-ty v-ty
                              :constraints (concat k-constr v-constr)
                              :pair-node new-pair})
               v-ctx]))
          [[] context]
          pairs)
        entries   (mapv (fn [{:keys [key-ty val-ty]}] [key-ty val-ty]) results)
        map-type  (t/make-hetero-map entries)
        new-pairs (mapv :pair-node results)
        new-node  (m/->Map new-pairs (:attrs node) (:meta node))
        all-constr (mapcat :constraints results)]
    [map-type (t/set-type! new-node map-type) all-constr final-ctx]))
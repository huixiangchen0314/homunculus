(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.map
  (:require
    [top.kzre.homunculus.core.ir2.ast :as m]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :map [current-node context]
  (let [pairs (:pairs current-node)      ;; Pair 向量
        [results final-ctx]
        (reduce
          (fn [[results ctx] pair-node]
            (let [k-node (:key pair-node)
                  v-node (:val pair-node)
                  {:keys [type node constraints env]}
                  (gen/gen-node* k-node ctx)
                  k-type        type
                  k-node'       node
                  k-constraints constraints
                  k-env         env
                  {:keys [type node constraints env]}
                  (gen/gen-node* v-node k-env)
                  new-pair (m/->Pair k-node' node
                                     (:attrs pair-node)
                                     (:meta pair-node))]
              [(conj results {:key-type    k-type
                              :val-type    type
                              :constraints (concat k-constraints constraints)
                              :pair-node   new-pair})
               env]))
          [[] context]
          pairs)
        entries    (mapv (fn [{:keys [key-type val-type]}] [key-type val-type]) results)
        map-type   (t/make-hetero-map entries)
        new-pairs  (mapv :pair-node results)
        new-node   (m/->Map new-pairs (:attrs current-node) (:meta current-node))
        all-constr (mapcat :constraints results)]
    {:type        map-type
     :node        (t/set-type! new-node map-type)
     :constraints all-constr
     :env         final-ctx}))
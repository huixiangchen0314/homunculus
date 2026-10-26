(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]))

(defmethod gen/gen-node* :let [current-node env]
  (let [[binding-results final-env] (gen/gen-seq (n/let-bindings current-node) env)
        bind-nodes (map :node binding-results)
        bind-constraints (mapcat :constraints binding-results)
        {:keys [type node constraints]}
        (gen/gen-node* (n/let-body current-node) final-env)
        new-node (n/make-let (vec bind-nodes) node
                             (n/attrs current-node)
                             (n/node-meta current-node))]
    (gen/result type new-node env
                bind-constraints constraints)))
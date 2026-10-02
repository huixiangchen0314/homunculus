(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :let [current-node env]
  (let [bindings (n/let-bindings current-node)   ;; Binding 向量
        [bind-nodes final-env bind-constraints]
        (reduce
          (fn [[bnds current-env collected] b]
            (let [var-node (:var b)
                  val-node (:val b)
                  {:keys [type node constraints]}
                  (gen/gen-node* val-node current-env)
                  var-name     (:name var-node)
                  binding-type (p/generalize current-env type)
                  typed-var    (t/set-type! var-node binding-type)
                  new-binding  (assoc b :var typed-var :val node)]
              [(conj bnds new-binding)
               (p/bind-var current-env var-name binding-type)
               (concat collected constraints)]))
          [[] env []]
          bindings)
        {:keys [type node constraints]}
        (gen/gen-node* (n/let-body current-node) final-env)
        new-node (n/make-let (vec bind-nodes) node
                             (n/attrs current-node)
                             (n/node-meta current-node))]
    {:type        type
     :node        (t/set-type! new-node type)
     :constraints (concat bind-constraints constraints)
     :env         env}))
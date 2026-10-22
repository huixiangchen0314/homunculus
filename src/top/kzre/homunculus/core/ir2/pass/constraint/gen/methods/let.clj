(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as p]
    [top.kzre.homunculus.core.ir2.pass.type :as t]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]))

(defmethod gen/gen-node* :let [current-node env]
  (let [bindings (n/let-bindings current-node)   ;; Binding 向量
        [bind-nodes final-env bind-constraints]
        (reduce
          (fn [[bnds current-env collected] b]
            (let [var-node (:var b)
                  var-name     (:name var-node)
                  hint-type (t/get-type var-node)
                  val-node (:val b)
                  {:keys [type node constraints]}
                  (gen/gen-node* val-node current-env)

                  binding-type (p/generalize current-env type)
                  typed-var    (if hint-type
                                 (t/set-type! var-node hint-type)
                                 (t/set-type! var-node binding-type))
                  collected' (if hint-type
                               (concat collected
                                       constraints
                                       [(cons/make-cequal hint-type binding-type)])
                              (concat collected constraints))
                  new-binding  (assoc b :var typed-var :val node)]
              [(conj bnds new-binding)
               (p/bind-var current-env var-name binding-type)
               collected']))
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
(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.let
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.pass.env :as e]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :let [current-node context]
  (let [bindings (n/let-bindings current-node)   ;; Binding 向量
        [bind-nodes final-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] b]
            (let [var-node (:var b)
                  val-node (:val b)
                  {:keys [type node constraints]}
                  (gen/gen-node* val-node (assoc context :env env))
                  var-name     (:name var-node)
                  binding-type (if (t/concrete? type)
                                 type
                                 (if (t/fun-type? type)
                                   (scheme/generalize type env)
                                   type))
                  typed-var    (t/set-type! var-node binding-type)
                  new-binding  (assoc b :var typed-var :val node)]
              [(conj bnds new-binding)
               (e/extend-env env var-name binding-type)
               (concat constrs constraints)]))
          [[] (u/env context) []]
          bindings)
        {:keys [type node constraints]}
        (gen/gen-node* (n/let-body current-node) (assoc context :env final-env))
        new-node (n/make-let (vec bind-nodes) node
                             (n/attrs current-node)
                             (n/node-meta current-node))]
    {:type        type
     :node        (t/set-type! new-node type)
     :constraints (concat bind-constraints constraints)
     :env         context}))
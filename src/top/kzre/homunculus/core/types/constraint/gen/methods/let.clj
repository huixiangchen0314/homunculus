(ns top.kzre.homunculus.core.types.constraint.gen.methods.let
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))


(defmethod gen/cg-node-raw :let [node context]
  (let [bindings (n/let-bindings node)      ;; 现在返回 Binding 向量
        [bind-nodes final-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] b]
            (let [var-node (:var b)
                  val-node (:val b)
                  [val-tv new-val val-constr _val-ctx] (gen/cg-node-raw val-node (assoc context :env env))
                  var-name (:name var-node)
                  binding-type (if (t/concrete? val-tv)
                                 val-tv
                                 (if (t/fun-type? val-tv)
                                   (scheme/generalize val-tv env)
                                   val-tv))
                  typed-var (t/set-type! var-node binding-type)
                  new-binding (assoc b :var typed-var :val new-val)]
              [(conj bnds new-binding)
               (e/extend-env env var-name binding-type)
               (concat constrs val-constr)]))
          [[] (u/env context) []]
          bindings)
        [body-tv body-node body-constr _body-ctx]
        (gen/cg-node-raw (n/let-body node) (assoc context :env final-env))
        new-node (n/make-let (vec bind-nodes) body-node
                             (n/attrs node) (n/node-meta node))]
    [body-tv
     (t/set-type! new-node body-tv)
     (concat bind-constraints body-constr)
     context]))
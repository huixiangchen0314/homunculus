(ns top.kzre.homunculus.core.types.constraint.gen.methods.let
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :let [node context]
  (let [bindings (n/let-bindings node)
        [bind-nodes new-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr] (gen/cg-node-raw val (assoc context :env env))
                  var-name (n/var-name var)
                  ;; 若 val-tv 已经是确定的具体类型，直接使用，不泛化
                  binding (if (ty/concrete? val-tv)
                            val-tv
                            ;; 否则若为函数类型，泛化为 TScheme，支持多态
                            (if (ty/fun-type? val-tv)
                              (scheme/generalize val-tv env)
                              val-tv))
                  var-node (ty/set-type! var binding)]
              [(conj bnds [var-node val-node])
               (e/extend-env env var-name binding)
               (concat constrs val-constr)]))
          [[] (:env context) []]
          bindings)
        [body-tv body-node body-constraints] (gen/cg-node-raw (n/let-body node) (assoc context :env new-env))]
    [body-tv
     (ty/set-type! (n/make-let (vec bind-nodes) body-node
                               (n/attrs node) (n/node-meta node) (n/parent node))
                   body-tv)
     (concat bind-constraints body-constraints)]))
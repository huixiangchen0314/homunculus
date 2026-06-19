(ns top.kzre.homunculus.core.types.constraint.gen.methods.let
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.types.env :as e]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :let [node context]
  (let [bindings (n/let-bindings node)
        ;; 顺序处理绑定，逐步扩展局部环境（仅内部可见）
        [bind-nodes final-env bind-constraints]
        (reduce
          (fn [[bnds env constrs] [var val]]
            (let [[val-tv val-node val-constr _val-ctx] (gen/cg-node-raw val (assoc context :env env))
                  var-name (n/var-name var)
                  binding (if (t/concrete? val-tv)
                            val-tv
                            (if (t/fun-type? val-tv)
                              (scheme/generalize val-tv env)
                              val-tv))
                  var-node (t/set-type! var binding)]
              [(conj bnds [var-node val-node])
               (e/extend-env env var-name binding)
               (concat constrs val-constr)]))
          [[] (u/env context) []]
          bindings)
        ;; 在累积的环境中推导 body
        [body-tv body-node body-constr _body-ctx] (gen/cg-node-raw (n/let-body node)
                                                                   (assoc context :env final-env))
        new-node (n/make-let (vec bind-nodes) body-node
                             (n/attrs node) (n/node-meta node) (n/parent node))]
    ;; 返回四元组：let 的类型是 body 类型，内部绑定不泄露，返回原上下文
    [body-tv
     (t/set-type! new-node body-tv)
     (concat bind-constraints body-constr)
     context]))
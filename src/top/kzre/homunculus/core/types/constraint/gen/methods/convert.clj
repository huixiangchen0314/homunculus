(ns top.kzre.homunculus.core.types.constraint.gen.methods.convert
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :convert [node context]
  (let [[expr-tv expr-node expr-constr] (gen/cg-node-raw (n/convert-expr node) context)
        src-ty (n/convert-src-ty node)
        dst-ty (n/convert-dst-ty node)
        tv dst-ty                                  ;; 整体类型即目标类型
        src-constr (list (c/make-cequal expr-tv src-ty))
        new-node (n/make-convert expr-node src-ty dst-ty (n/convert-cost node)
                                 (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (ty/set-type! new-node tv) (concat expr-constr src-constr)]))
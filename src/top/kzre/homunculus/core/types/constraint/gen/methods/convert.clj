(ns top.kzre.homunculus.core.types.constraint.gen.methods.convert
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :convert [node context]
  ;; 推断要转换的表达式，获得四元组
  (let [[expr-tv expr-node expr-constr expr-ctx] (gen/cg-node-raw (n/convert-expr node) context)
        src-ty (n/convert-src-ty node)
        dst-ty (n/convert-dst-ty node)
        ;; 整体类型即为目标类型
        tv dst-ty
        ;; 表达式类型必须等于源类型
        src-constr (when (and expr-tv src-ty)
                     [(c/make-cequal expr-tv src-ty)])
        new-node (n/make-convert expr-node src-ty dst-ty (n/convert-cost node)
                                 (n/attrs node) (n/node-meta node) (n/parent node))]
    [tv (t/set-type! new-node tv) (concat expr-constr src-constr) expr-ctx]))
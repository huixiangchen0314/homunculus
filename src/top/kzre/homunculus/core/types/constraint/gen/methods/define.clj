(ns top.kzre.homunculus.core.types.constraint.gen.methods.define
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as t]))

(defmethod gen/cg-node-raw :define [node context]
  (let [[val-tv val-node val-constr val-ctx] (gen/cg-node-raw (n/define-val node) context)
        ;; 用户标注类型（若存在则必定为 TCon，无需额外判断）
        annotated-ty (t/get-type node (u/known-types context))
        final-tv     (or annotated-ty val-tv)
        extra-constr (when annotated-ty
                       [(c/make-cequal val-tv annotated-ty)])
        new-node     (n/make-define (n/define-name node)
                                    val-node
                                    (n/define-doc node)
                                    (n/attrs node)
                                    (n/node-meta node)
                                    (n/parent node))
        ;; 将定义名与最终类型写入环境
        new-ctx      (u/extend-env val-ctx (n/define-name node) final-tv)]
    [final-tv (t/set-type! new-node final-tv)
     (concat val-constr extra-constr)
     new-ctx]))
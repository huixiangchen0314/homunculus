(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.convert
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :convert [current-node context]
  (let [{:keys [type node constraints env]} (gen/gen-node* (n/convert-expr current-node) context)
        src-ty   (n/convert-src-ty current-node)
        dst-ty   (n/convert-dst-ty current-node)
        ;; 表达式类型必须等于源类型
        src-eq   (when (and type src-ty)
                   [(cons/make-cequal type src-ty)])
        new-node (n/make-convert node src-ty dst-ty
                                 (n/attrs current-node)
                                 (n/node-meta current-node))]
    {:type        dst-ty
     :node        (t/set-type! new-node dst-ty)
     :constraints (concat constraints src-eq)
     :env         env}))
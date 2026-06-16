(ns top.kzre.homunculus.core.types.check.methods.convert
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check-node :convert [node expected context]
  ;; 转换节点的目标类型即 dst-ty，内部 expr 应满足 src-ty 约束（已由约束求解保证）
  ;; 这里只需递归检查内部 expr，期望类型用 dst-ty（或 expected）
  (let [dst-ty (n/convert-dst-ty node)
        ;; 对内部 expr 传递 dst-ty 作为期望
        new-expr (check/check-node (n/convert-expr node) (or expected dst-ty) context)]
    (n/make-convert new-expr
                    (n/convert-src-ty node)
                    dst-ty
                    (n/convert-cost node)
                    (n/attrs node) (n/node-meta node) (n/parent node))))
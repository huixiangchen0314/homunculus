(ns top.kzre.homunculus.core.ir2.pass.infer.methods.convert
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]))

(defmethod infer/infer-node* :convert [node env]
  ;; 推断被转换的表达式，累积环境
  (let [[_ new-expr expr-env] (infer/infer-node* (n/convert-expr node) env)
        ;; 重建 convert 节点，保留原有类型转换信息
        new-node (n/make-convert new-expr
                                 (n/convert-src-ty node)
                                 (n/convert-dst-ty node)
                                 (n/attrs node)
                                 (n/node-meta node))]
    ;; convert 节点本身无类型，返回新节点及传递后的环境
    (infer/nothing new-node expr-env)))
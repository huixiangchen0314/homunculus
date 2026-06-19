(ns top.kzre.homunculus.core.types.constraint.gen.methods.while
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.types.constraint.constraint :as c]
            [top.kzre.homunculus.core.types.constraint.utils :as u]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :while [node context]
  ;; 1. 推导 test 表达式
  (let [[test-tv test-node test-constr test-ctx] (gen/cg-node-raw (n/while-test node) context)
        ;; 2. 推导 body，使用 test 后的上下文
        [body-tv body-node body-constr body-ctx] (gen/cg-node-raw (n/while-body node) test-ctx)
        ;; while 整体类型为 nil
        tv (ty/make-tcon 'nil)
        ;; 根据前端策略添加 test 真值类型约束
        test-eq (when-let [req-ty (u/truthy-type-requirement context)]
                  (when test-tv
                    [(c/make-cequal test-tv (ty/make-tcon req-ty))]))
        new-node (n/make-while test-node body-node
                               (n/attrs node) (n/node-meta node) (n/parent node))]
    ;; 返回四元组：类型、新节点、约束、最终上下文
    [tv (ty/set-type! new-node tv)
     (concat test-constr body-constr test-eq)
     body-ctx]))
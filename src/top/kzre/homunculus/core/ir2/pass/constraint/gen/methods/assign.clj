(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.assign
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :assign [current-node context]
  ;; 1. 推导左侧变量
  (let [{var-type        :type
         var-node        :node
         var-constraints :constraints
         var-env         :env}
        (gen/gen-node* (n/assign-var current-node) context)
        ;; 2. 推导右侧值，使用变量推导后的环境
        {val-type        :type
         val-node        :node
         val-constraints :constraints
         val-env         :env}
        (gen/gen-node* (n/assign-val current-node) var-env)
        ;; 赋值表达式类型与左右两侧一致
        all-constr (concat var-constraints val-constraints
                           (when (and var-type val-type)
                             [(cons/make-cequal var-type val-type)]))
        new-node   (t/set-type! (n/make-assign var-node val-node
                                               (n/attrs current-node)
                                               (n/node-meta current-node))
                                var-type)]
    {:type        var-type
     :node        new-node
     :constraints all-constr
     :env         val-env}))
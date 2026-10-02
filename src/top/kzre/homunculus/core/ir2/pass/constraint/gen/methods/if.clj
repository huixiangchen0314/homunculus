(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.if
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :if [current-node context]
  (let [{test-type        :type
         test-node        :node
         test-constraints :constraints
         test-env         :env}
        (gen/gen-node* (n/if-test current-node) context)
        {then-type        :type
         then-node        :node
         then-constraints :constraints
         then-env         :env}
        (gen/gen-node* (n/if-then current-node) test-env)
        {else-type        :type
         else-node        :node
         else-constraints :constraints
         else-env         :env}
        (if-let [else (n/if-else current-node)]
          (gen/gen-node* else then-env)
          {:type        nil
           :node        nil
           :constraints nil
           :env         then-env})
        tv        (gen/fresh-tvar)
        ;; 如果有明确的真值类型，添加约束
        test-eq   (when-let [required-type (u/truthy-type-requirement context)]
                    (when test-type
                      [(cons/make-cequal test-type (t/make-tcon required-type))]))
        ;; 分支类型必须一致（静态语言标准行为）
        branch-eq (cond-> []
                          then-type (conj (cons/make-cequal then-type tv))
                          else-type (conj (cons/make-cequal else-type tv)))
        new-node  (n/make-if test-node then-node else-node
                             (n/attrs current-node)
                             (n/node-meta current-node))]
    {:type        tv
     :node        (t/set-type! new-node tv)
     :constraints (concat test-constraints then-constraints else-constraints
                          test-eq branch-eq)
     :env         else-env}))
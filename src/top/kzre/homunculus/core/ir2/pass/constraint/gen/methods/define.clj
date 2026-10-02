(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.define
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.constraints.core :as cons]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.constraint.utils :as u]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :define [current-node context]
  (if (true? (:ho? (n/attrs current-node)))
    ;; 高阶函数：不处理值节点，分配自由类型变量，不生成约束，不写入环境
    (let [tv       (gen/fresh-tvar)
          new-node (n/make-define (n/define-name current-node)
                                  (n/define-val current-node)   ; 保持原值不变
                                  (n/define-docstring current-node)
                                  (n/attrs current-node)
                                  (n/node-meta current-node))]
      {:type        tv
       :node        (t/set-type! new-node tv)
       :constraints nil
       :env         context})

    (let [{:keys [type node constraints env]}
          (gen/gen-node* (n/define-val current-node) context)
          annotated-type (t/get-type current-node)
          final-type     (or annotated-type type)
          extra-constr   (when annotated-type
                           [(cons/make-cequal type annotated-type)])
          new-node       (n/make-define (n/define-name current-node)
                                        node
                                        (n/define-docstring current-node)
                                        (n/attrs current-node)
                                        (n/node-meta current-node))
          ;; 将定义名与最终类型写入环境
          new-env        (u/extend-env env (n/define-name current-node) final-type)]
      {:type        final-type
       :node        (t/set-type! new-node final-type)
       :constraints (concat constraints extra-constr)
       :env         new-env})))
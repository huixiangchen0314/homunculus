(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.block
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod gen/gen-node* :block [current-node context]
  (let [exprs     (n/block-exprs current-node)
        ;; 顺序处理每个表达式，用 gen-node（含注解处理），环境随遍历推进
        [results final-env]
        (reduce (fn [[results current-env] expr]
                  (let [{:keys [env] :as result} (gen/gen-node expr current-env)]
                    [(conj results result) env]))
                [[] context]
                exprs)
        types       (mapv :type results)
        new-exprs   (mapv :node results)
        constraints (mapcat :constraints results)
        ;; 块类型取最后一个表达式的类型；空块分配新类型变量
        block-type  (if (seq types)
                      (last types)
                      (gen/fresh-tvar))
        new-node    (n/make-block new-exprs
                                  (n/attrs current-node)
                                  (n/node-meta current-node))]
    {:type        block-type
     :node        (t/set-type! new-node block-type)
     :constraints constraints
     :env         final-env}))
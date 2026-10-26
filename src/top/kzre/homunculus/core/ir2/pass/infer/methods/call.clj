(ns top.kzre.homunculus.core.ir2.pass.infer.methods.call
  (:require
   [top.kzre.homunculus.core.ir2.node :as n]
   [clojure.string :as str]
   [top.kzre.homunculus.core.ir2.pass.constraint.gen.env :as env]
   [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod infer/infer-node* :call [node env]
  ;; 1. 递归推导被调用的函数部分，得到函数类型、新节点和新环境
  (let [[fn-type fn-node fn-env] (infer/infer-node* (n/call-fn node) env)
        ;; 2. 顺序处理所有实参，累积环境
        [arg-nodes  final-env]
        (infer/infer-seq (n/call-args node) fn-env)
        fn-name     (when (= (n/kind fn-node) :variable)
                      (n/var-name fn-node))
        candidates  (cond
                      (and fn-type (ty/fun-type? fn-type)) [fn-type]
                      fn-name                             (env/resolve-callees fn-env fn-name)
                      :else                               [])
        new-node (n/call-with-children node fn-node arg-nodes)
        ]
    (if-let [t     ;; 所有候选项目的返回值相同，且是确定的
             (when (seq candidates)
               (let [return-types (distinct (map ty/fun-return-type candidates)) ]
                 (when (= 1 (count return-types))
                   (let [cand (first return-types)]
                     (when (ty/concrete? cand)
                       cand)))))]
      (infer/success t new-node final-env)
      (infer/nothing (n/call-with-children node fn-node arg-nodes) final-env))))
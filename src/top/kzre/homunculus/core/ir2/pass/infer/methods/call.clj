(ns top.kzre.homunculus.core.ir2.pass.infer.methods.call
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.type :as t]))

(defmethod infer/infer-node* :call [node env]
  ;; 1. 递归推导被调用的函数部分，得到函数类型、新节点和新环境
  (let [[fn-type fn-node fn-env] (infer/infer-node* (n/call-fn node) env)
        ;; 2. 顺序处理所有实参，累积环境
        [arg-nodes arg-types final-env]
        (reduce (fn [[nodes types current-env] arg]
                  (let [[arg-type arg-node arg-env] (infer/infer-node* arg current-env)]
                    [(conj nodes arg-node) (conj types arg-type) arg-env]))
                [[] [] fn-env]
                (n/call-args node))]
    ;; 3. 推导成功需同时满足：
    ;;    (a) 函数部分有类型（fn-type 非 nil）
    ;;    (b) 所有实参都有推导出的类型
    ;;    (c) 函数类型确实是 TFun，包含参数和返回类型信息
    (if (and fn-type
             (every? some? arg-types)
             (t/fun-type? fn-type))
      ;; —— 成功路径 ——
      (let [ret-type     (t/fun-return-type fn-type (count arg-types))
            updated-node (-> node
                             (n/call-with-children fn-node arg-nodes)
                             (t/set-type! ret-type))]
        (infer/success ret-type updated-node final-env))
      ;; —— 失败路径 ——
      ;; 保留子节点的推导结果，但不标注本节点类型
      (infer/nothing (n/call-with-children node fn-node arg-nodes) final-env))))
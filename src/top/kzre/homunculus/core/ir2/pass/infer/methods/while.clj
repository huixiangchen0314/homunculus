(ns top.kzre.homunculus.core.ir2.pass.infer.methods.while
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.infer.core :as infer]
    [top.kzre.homunculus.core.ir2.pass.type :as type]))

(defmethod infer/infer-node* :while [node env]
  ;; 1. 推导条件表达式
  (let [[_ test-node test-env]   (infer/infer-node* (n/while-test node) env)
        ;; 2. 推导循环体，使用条件推导后的环境
        [body-type body-node body-env] (infer/infer-node* (n/while-body node) test-env)
        new-node (n/while-with-children node test-node body-node)]
    ;; 3. while 整体类型为 body 的类型
    (if body-type
      (infer/success body-type (type/set-type! new-node body-type) body-env)
      (infer/nothing new-node body-env))))
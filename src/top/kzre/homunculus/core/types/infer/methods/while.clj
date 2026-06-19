(ns top.kzre.homunculus.core.types.infer.methods.while
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as type]))

(defmethod infer/local-infer :while [node context]
  ;; 1. 推导条件表达式
  (let [[_test-ty test-node test-ctx] (infer/local-infer (n/while-test node) context)
        ;; 2. 推导循环体，使用条件推导后的上下文
        [body-ty body-node body-ctx] (infer/local-infer (n/while-body node) test-ctx)]
    ;; 3. 成功条件：循环体有推导出的类型
    ;;    while 整体类型为 body 的类型
    (if body-ty
      ;; —— 成功路径 ——
      (infer/success body-ty
                     (-> node
                         (n/while-with-children test-node body-node)
                         (type/set-type! body-ty))
                     body-ctx)
      ;; —— 失败路径 ——
      (infer/nothing (n/while-with-children node test-node body-node) body-ctx))))
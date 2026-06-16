(ns top.kzre.homunculus.core.ir1.forms.fn
  "fn* 特殊形式的 IR1 构建。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'fn* [form]
  (let [[_ maybe-name params & body] form
        [name params body] (if (symbol? maybe-name)
                             [maybe-name params body]
                             [nil maybe-name (cons params body)])
        ;; 将参数符号转换为参数描述 map
        param-descs (mapv (fn [p] (n/make-param p (meta p))) params)]
    (n/make-fn name param-descs body (meta form))))

(defmethod ir1/build-tree :fn [node]
  (let [name      (n/fn-name node)          ;; 可能是 nil 或符号
        params    (n/fn-params node)        ;; 参数描述列表
        body      (n/fn-body node)          ;; 原始表单向量（来自 form->node）
        meta      (n/node-meta node)
        parent    (n/parent node)
        ;; 将参数描述中的符号构建为 IR1 节点
        param-iris (mapv (fn [p] (ir1/->ir1 (n/param-sym p))) params)
        ;; 递归构建函数体，得到子节点向量
        body-iris  (mapv ir1/->ir1 body)
        ;; 包装函数体为单个 DoNode（如果多个）或保持单个节点
        wrapped-body (n/wrap-body body-iris)
        ;; 如果函数有名字，构建名字节点 (SymbolNode)
        name-node (when name (ir1/->ir1 name))]
    ;; 构造新的 FnNode，body 现在是单个节点（可能是 DoNode）
    (n/make-fn name-node param-iris wrapped-body meta)))
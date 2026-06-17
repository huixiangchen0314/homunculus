(ns top.kzre.homunculus.core.ir1.forms.ns
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node 'ns [form]
  (let [[_ name & rest-args] form
        ;; 从剩余参数中提取 docstring、attr-map 和 references
        docstring   (when (string? (first rest-args)) (first rest-args))
        after-doc   (if docstring (rest rest-args) rest-args)
        attr-map    (when (map? (first after-doc)) (first after-doc))
        references  (if attr-map (second after-doc) (first after-doc))]
    (n/make-ns name docstring attr-map references (meta form))))

(defmethod ir1/build-tree :ns [node]
  ;; 重建节点以附加 parent 和保持不可变性
  (n/make-ns (n/namespace-name node)
             (n/namespace-docstring node)
             (n/namespace-attr-map node)
             (n/namespace-references node)
             (n/node-meta node)
             (n/parent node)))
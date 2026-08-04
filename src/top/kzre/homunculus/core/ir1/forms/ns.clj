(ns top.kzre.homunculus.core.ir1.forms.ns
  "ns 形式的 IR1 构建，提取可选的 docstring、attr-map 和 :require 子句。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'ns [form]
  (let [[_ name & rest-args] form
        first-arg (first rest-args)
        ;; 1. 提取 docstring（如果存在）
        [docstring after-doc] (if (string? first-arg)
                                [first-arg (rest rest-args)]
                                [nil rest-args])
        ;; 2. 提取 attr-map（可选）
        [attr-map after-attr] (if (map? (first after-doc))
                                [(first after-doc) (rest after-doc)]
                                [nil after-doc])
        ;; 3. 检查是否有 (:require ...) 列表
        [require-form remaining] (if-let [rf (first after-attr)]
                                   (if (and (seq? rf) (= (first rf) :require))
                                     [rf (rest after-attr)]
                                     [nil after-attr])
                                   [nil after-attr])
        ;; 4. 提取 require 规格，若无则为空向量
        requires (if require-form
                   (vec (rest require-form))
                   [])
        ;; 5. 合并 attr-map 和原始 meta
        merged-meta (merge (meta form) attr-map)]
    (m/->Ns name requires docstring merged-meta)))
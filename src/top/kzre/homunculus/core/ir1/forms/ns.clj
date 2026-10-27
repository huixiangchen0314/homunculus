(ns top.kzre.homunculus.core.ir1.forms.ns
  "ns 形式的 IR1 构建，提取可选的 docstring、attr-map 和 :require 子句。"
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.ast :as m]))

(defmethod ir1/lower-node* 'ns [form env]
  (let [[_ name & rest-args] form
        first-arg (first rest-args)
        [docstring after-doc] (if (string? first-arg)
                                [first-arg (rest rest-args)]
                                [nil rest-args])
        [attr-map after-attr] (if (map? (first after-doc))
                                [(first after-doc) (rest after-doc)]
                                [nil after-doc])
        [require-form _remaining] (if-let [rf (first after-attr)]
                                    (if (and (seq? rf) (= (first rf) :require))
                                      [rf (rest after-attr)]
                                      [nil after-attr])
                                    [nil after-attr])
        requires    (if require-form (vec (rest require-form)) [])
        merged-meta (merge (meta form) attr-map)]
    [(m/->Ns name requires docstring merged-meta) env]))
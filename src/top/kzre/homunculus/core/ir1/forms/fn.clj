;; ir1/forms/fn.clj
(ns top.kzre.homunculus.core.ir1.forms.fn
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'fn* [form]
  (let [[_ maybe-name params & body] form
        [name params body] (if (symbol? maybe-name)
                             [maybe-name params body]
                             [nil maybe-name (cons params body)])]
    (m/->FnNode name (mapv (fn [p] {:sym p :meta (meta p)}) params) body nil [] nil)))

(defmethod ir1/build-tree :fn [node]
  (let [name (:name node)
        params (:params node)
        body   (:body node)
        param-iris (mapv #(ir1/->ir1 (:sym %)) params)
        body-iris  (mapv ir1/->ir1 body)
        children (if name
                   (into [(ir1/->ir1 name)] (concat param-iris body-iris))
                   (into param-iris body-iris))]
    (assoc node :children children)))
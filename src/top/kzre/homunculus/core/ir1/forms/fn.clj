(ns top.kzre.homunculus.core.ir1.forms.fn
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'fn* [form]
  (let [[_ maybe-name params & body] form
        [name params body] (if (symbol? maybe-name)
                             [maybe-name params body]
                             [nil maybe-name (cons params body)])]
    (m/->FnNode name (mapv (fn [p] {:sym p :meta (meta p)}) params) body (meta form) nil)))

(defmethod ir1/build-tree :fn [node]
  (let [name (:name node)
        params (:params node)
        body   (:body node)
        param-iris (mapv #(ir1/->ir1 (:sym %)) params)
        body-iris  (mapv ir1/->ir1 body)]
    (if name
      (m/->FnNode (ir1/->ir1 name) param-iris body-iris (:meta node) (:parent node))
      (m/->FnNode nil param-iris body-iris (:meta node) (:parent node)))))
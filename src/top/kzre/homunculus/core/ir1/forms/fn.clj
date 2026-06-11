(ns top.kzre.homunculus.core.ir1.forms.fn
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'fn* [form]
  (let [[_ maybe-name params & body] form]
    (if (symbol? maybe-name)
      (ir1/make-node :fn* :name maybe-name
                     :params (mapv (fn [p] {:sym p :meta (meta p)}) params)
                     :body body)
      (ir1/make-node :fn* :name nil
                     :params (mapv (fn [p] {:sym p :meta (meta p)}) maybe-name)
                     :body (cons params body)))))

(defmethod ir1/parse-form :fn* [node]
  (let [params-vec (:params node)
        body (:body node)
        name (:name node)
        name-ir (when name (ir1/->ir1 name))
        param-irs (mapv #(ir1/->ir1 (:sym %)) params-vec)
        body-irs (mapv ir1/->ir1 body)]
    (vec (cons node (remove nil? (concat (when name-ir [name-ir])
                                         param-irs
                                         body-irs))))))
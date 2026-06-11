(ns top.kzre.homunculus.core.ir1.forms.let
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'let* [form]
  (let [[_ bindings & body] form]
    (ir1/make-node :let* :bindings bindings :body body)))

(defmethod ir1/parse-form :let* [node]
  (let [bindings (:bindings node)
        pair-count (/ (count bindings) 2)
        binding-irs (mapv (fn [i]
                            (let [sym (nth bindings (* 2 i))
                                  val (nth bindings (inc (* 2 i)))]
                              [(ir1/->ir1 sym) (ir1/->ir1 val)]))
                          (range pair-count))
        body-irs (mapv ir1/->ir1 (:body node))]
    (vec (cons node (concat (apply concat binding-irs) body-irs)))))
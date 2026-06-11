(ns top.kzre.homunculus.core.ir1.forms.loop
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'loop* [form]
  (let [[_ bindings & body] form]
    (ir1/make-node :loop :bindings bindings :body body)))

(defmethod ir1/form->node 'recur [form]
  (let [[_ & exprs] form]
    (ir1/make-node :recur :exprs exprs)))

(defmethod ir1/parse-form :loop [node]
  (let [bindings (:bindings node)
        pair-count (/ (count bindings) 2)
        binding-irs (mapv (fn [i]
                            (let [sym (nth bindings (* 2 i))
                                  val (nth bindings (inc (* 2 i)))]
                              [(ir1/->ir1 sym) (ir1/->ir1 val)]))
                          (range pair-count))
        body-irs (mapv ir1/->ir1 (:body node))]
    (vec (cons node (concat (apply concat binding-irs) body-irs)))))

(defmethod ir1/parse-form :recur [node]
  (let [expr-irs (mapv ir1/->ir1 (:exprs node))]
    (vec (cons node expr-irs))))
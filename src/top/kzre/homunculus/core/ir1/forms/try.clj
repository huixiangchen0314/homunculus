(ns top.kzre.homunculus.core.ir1.forms.try
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/lower-node* 'throw [form env]
  (let [[_ expr] form
        [expr-node env'] (ir1/lower-node expr env)]
    [(n/make-throw expr-node (meta form)) env']))

(defn- make-catch-node
  [clause env]
  (let [[_ class sym & body] clause
        env-catch    (ir1/bind-local-symbol env sym)
        [body-nodes _] (ir1/lower-seq body env-catch)]
    (n/make-catch class sym (n/wrap-body body-nodes) (meta clause))))

(defmethod ir1/lower-node* 'try [form env]
  (let [[_ & body-parts] form
        body         (take-while #(not (contains? #{'catch 'finally} (first %))) body-parts)
        after-body   (drop (count body) body-parts)
        catches      (take-while #(= 'catch (first %)) after-body)
        finally-part (drop (count catches) after-body)
        finally-expr (when (and (seq finally-part)
                                (= 'finally (ffirst finally-part)))
                       (rest (first finally-part)))
        [body-nodes _]    (ir1/lower-seq body env)
        ir-body           (n/wrap-body body-nodes)
        ir-catches        (mapv #(make-catch-node % env) catches)
        [finally-nodes _] (when (seq finally-expr)
                            (ir1/lower-seq finally-expr env))
        ir-finally        (when (seq finally-expr)
                            (n/wrap-body finally-nodes))]
    [(n/make-try ir-body ir-catches ir-finally (meta form)) env]))
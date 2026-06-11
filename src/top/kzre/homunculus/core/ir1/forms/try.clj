(ns top.kzre.homunculus.core.ir1.forms.try
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]))

(defmethod ir1/form->node 'try [form]
  (let [[_ & body-parts] form
        body (take-while #(not (contains? #{'catch 'finally} (first %))) body-parts)
        after-body (drop (count body) body-parts)
        catches (take-while #(= 'catch (first %)) after-body)
        finally-part (drop (count catches) after-body)
        finally-expr (when (= 'finally (ffirst finally-part))
                       (rest (first finally-part)))]
    (ir1/make-node :try :body body
                   :catches (mapv rest catches)
                   :finally finally-expr)))

(defmethod ir1/parse-form :try [node]
  (let [body-irs   (mapv ir1/->ir1 (:body node))
        catch-irs  (mapv (fn [catch-clause]
                           (let [class-ir (ir1/->ir1 (first catch-clause))
                                 sym-ir   (ir1/->ir1 (second catch-clause))
                                 body-exprs (nthrest catch-clause 2)
                                 body-irs  (mapv ir1/->ir1 body-exprs)]
                             (vec (cons (ir1/make-node :catch :class (:class class-ir) :sym (:sym sym-ir))
                                        (concat [class-ir sym-ir] body-irs)))))
                         (:catches node))
        finally-irs (when-let [fexpr (:finally node)]
                      (mapv ir1/->ir1 fexpr))]
    (vec (cons node (concat body-irs catch-irs finally-irs)))))

(defmethod ir1/parse-form :catch [node] [node])
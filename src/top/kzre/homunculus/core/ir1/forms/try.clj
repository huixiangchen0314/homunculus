(ns top.kzre.homunculus.core.ir1.forms.try
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.model :as m]))

(defmethod ir1/form->node 'try [form]
  (let [[_ & body-parts] form
        body (take-while #(not (contains? #{'catch 'finally} (first %))) body-parts)
        after-body (drop (count body) body-parts)
        catches (take-while #(= 'catch (first %)) after-body)
        finally-part (drop (count catches) after-body)
        finally-expr (when (= 'finally (ffirst finally-part)) (rest (first finally-part)))]
    (m/->TryNode (vec body) (mapv rest catches) finally-expr (meta form) nil)))

(defmethod ir1/build-tree :try [node]
  (let [body-irs    (mapv ir1/->ir1 (:body node))
        catch-irs   (mapv (fn [c]
                            (m/->CatchNode (ir1/->ir1 (first c))
                                           (ir1/->ir1 (second c))
                                           (mapv ir1/->ir1 (nthrest c 2))
                                           nil nil))   ;; 后续 attach-parents 会设置 parent
                          (:catches node))
        finally-irs (when-let [f (:finally node)]
                      (mapv ir1/->ir1 f))]
    (m/->TryNode body-irs catch-irs finally-irs (:meta node) (:parent node))))
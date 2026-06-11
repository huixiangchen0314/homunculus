;; ir1/forms/try.clj
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
    (m/->TryNode (vec body) (mapv rest catches) finally-expr nil [] nil)))

(defmethod ir1/build-tree :try [node]
  (let [body-irs   (mapv ir1/->ir1 (:body node))
        catch-irs  (mapv (fn [c]
                           (let [class-node (ir1/->ir1 (first c))
                                 sym-node   (ir1/->ir1 (second c))
                                 body-nodes (mapv ir1/->ir1 (nthrest c 2))]
                             ;; children 必须是扁平的 INode 向量
                             (m/->CatchNode (first c) (second c) (nthrest c 2) nil
                                            (vec (concat [class-node sym-node] body-nodes))
                                            nil)))
                         (:catches node))
        finally-irs (when-let [f (:finally node)] (mapv ir1/->ir1 f))
        children (vec (concat body-irs catch-irs (or finally-irs [])))]
    (assoc node :children children)))
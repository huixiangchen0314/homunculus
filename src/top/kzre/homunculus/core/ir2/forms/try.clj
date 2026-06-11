;; ═══════════════════════════════════════════════════════
;; ir2/forms/try.clj
;; ═══════════════════════════════════════════════════════
(ns top.kzre.homunculus.core.ir2.forms.try
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :try [node env]
  (let [node-meta (ir2/ir1-meta node)
        kids (ir1p/children node)
        ;; 分离 body / catch / finally
        body-irs (take-while #(not= (ir1p/kind %) :catch) kids)
        rest-after-body (drop (count body-irs) kids)
        catch-irs (take-while #(= (ir1p/kind %) :catch) rest-after-body)
        finally-irs (drop (count catch-irs) rest-after-body)
        body-nodes (mapv #(first (ir2/lower-ast % env)) body-irs)
        catch-nodes (mapv (fn [c]
                            (let [catch-kids (ir1p/children c)
                                  class-ir (first catch-kids)
                                  sym-ir (second catch-kids)
                                  body-exprs (drop 2 catch-kids)
                                  class-node (first (ir2/lower-ast class-ir env))
                                  sym-node (first (ir2/lower-ast sym-ir env))
                                  c-body-nodes (mapv #(first (ir2/lower-ast % env)) body-exprs)]
                              (m/->CatchNode class-node sym-node c-body-nodes nil nil
                                             (vec (cons class-node (cons sym-node c-body-nodes)))
                                             nil)))
                          catch-irs)
        finally-nodes (when (seq finally-irs)
                        (mapv #(first (ir2/lower-ast % env)) finally-irs))
        children (vec (concat body-nodes catch-nodes (or finally-nodes [])))]
    [(m/->TryNode body-nodes catch-nodes finally-nodes nil node-meta children nil)]))

(defmethod ir2/lower-ast :throw [node env]
  (let [kid (first (ir1p/children node))
        expr (first (ir2/lower-ast kid env))]
    [(m/->ThrowNode expr nil (ir2/ir1-meta node) [expr] nil)]))
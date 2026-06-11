;; ═══════════════════════════════════════════════════════
;; ir2/forms/loop.clj
;; ═══════════════════════════════════════════════════════
(ns top.kzre.homunculus.core.ir2.forms.loop
  (:require [top.kzre.homunculus.core.ir1.protocol :as ir1p]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as m]))

(defmethod ir2/lower-ast :loop* [node env]
  (let [node-meta (ir2/ir1-meta node)
        bind-count (:bindings-count node)
        kids (ir1p/children node)
        bind-kids (take (* 2 bind-count) kids)
        body-kids (drop (* 2 bind-count) kids)
        bind-pairs (mapv (fn [[s v]]
                           [(first (ir2/lower-ast s env))
                            (first (ir2/lower-ast v env))])
                         (partition 2 bind-kids))
        body (if (= 1 (count body-kids))
               (first (ir2/lower-ast (first body-kids) env))
               (let [body-nodes (mapv #(first (ir2/lower-ast % env)) body-kids)]
                 (m/->BlockNode body-nodes nil nil body-nodes nil)))
        children (vec (concat (apply concat bind-pairs) [body]))]
    [(m/->LoopNode bind-pairs body nil node-meta children nil)]))

(defmethod ir2/lower-ast :recur [node env]
  (let [args (mapv #(first (ir2/lower-ast % env)) (ir1p/children node))]
    [(m/->RecurNode args nil (ir2/ir1-meta node) args nil)]))
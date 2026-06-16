(ns top.kzre.homunculus.core.types.lambda-elim.methods.call
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :call [node roots config defs]
  (let [fn-node (elim/eliminate (n/call-fn node) roots config defs)
        args    (mapv #(elim/eliminate % roots config defs) (n/call-args node))]
    (if-let [idx (first (keep-indexed (fn [i a] (when (= :lambda (n/kind a)) i)) args))]
      (let [lam (nth args idx)
            {:keys [new-call new-defines]} (elim/monomorphize
                                             (n/make-call fn-node args (n/attrs node) (n/node-meta node) nil)
                                             lam idx roots config)]
        (swap! defs into new-defines)
        new-call)
      (n/make-call fn-node args (n/attrs node) (n/node-meta node) (n/parent node)))))
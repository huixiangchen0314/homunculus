(ns top.kzre.homunculus.core.types.typed.methods.if
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defn- infer-branch [branch context]
  (if branch
    (let [[ty node s] (infer/infer branch context)]
      [ty node s])
    [nil nil {}]))

(defmethod infer/infer :if [node context]
  (if-let [existing (get-in node [:attrs :type])]
    [existing node {}]
    (let [[test-ty test-node s-test] (infer/infer (:test node) context)
          _ (u/unify test-ty (t/->TCon :bool))
          [then-ty then-node s-then] (infer/infer (:then node) context)
          [else-ty else-node s-else] (infer-branch (:else node) context)]
      (when (and else-ty (not= (ir2p/kind (:then node)) :recur))
        (u/unify then-ty else-ty))
      (let [overall-ty (if (= (ir2p/kind (:then node)) :recur) else-ty then-ty)
            s (merge s-test s-then s-else)
            new-attrs (assoc (ir2p/attrs node) :type overall-ty)]
        [overall-ty (assoc node :test test-node :then then-node :else else-node :attrs new-attrs) s]))))
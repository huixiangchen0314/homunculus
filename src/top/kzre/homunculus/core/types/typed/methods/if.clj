(ns top.kzre.homunculus.core.types.typed.methods.if
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.unify :as u]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defn- infer-branch [branch context]
  (if branch
    (let [[ty node] (infer/infer branch context)]
      [ty node])
    [nil nil]))

(defmethod infer/infer :if [node context]
  (let [[test-ty test-node] (infer/infer (:test node) context)
        _ (u/unify test-ty (t/->TCon :bool))
        [then-ty then-node] (infer/infer (:then node) context)
        [else-ty else-node] (infer-branch (:else node) context)]
    ;; 如果 then 分支是 recur，则跳过统一，整体类型取 else
    (when (and else-ty (not= (ir2p/kind (:then node)) :recur))
      (u/unify then-ty else-ty))
    (let [overall-ty (if (= (ir2p/kind (:then node)) :recur) else-ty then-ty)
          new-attrs (assoc (ir2p/attrs node) :type overall-ty)
          new-node (-> node
                       (assoc :test test-node :then then-node :else else-node)
                       (assoc :attrs new-attrs))]
      [overall-ty new-node])))
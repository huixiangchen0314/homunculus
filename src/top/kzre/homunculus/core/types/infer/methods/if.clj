(ns top.kzre.homunculus.core.types.infer.methods.if
  (:require [top.kzre.homunculus.core.types.infer.core :as infer]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.type :as type]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p])
  (:import [top.kzre.homunculus.core.types.model TVar TCon TFun]))

(defn- infer-branch [branch context]
  (if branch
    (infer/local-infer branch context)
    [nil nil]))

(defmethod infer/local-infer :if [node context]
  (let [[test-ty test-node] (infer/local-infer (:test node) context)
        [then-ty then-node] (infer/local-infer (:then node) context)
        [else-ty else-node] (infer-branch (:else node) context)]
    (if (and test-ty (instance? TCon test-ty) (= (:name test-ty) :bool))
      (if (and else-ty (= then-ty else-ty))
        (infer/success then-ty
                       (-> node
                           (assoc :test test-node :then then-node :else else-node)
                           (type/set-type! then-ty)))
        (infer/nothing (-> node
                           (assoc :test test-node :then then-node :else else-node))))
      (infer/nothing (-> node
                         (assoc :test test-node :then then-node :else else-node))))))
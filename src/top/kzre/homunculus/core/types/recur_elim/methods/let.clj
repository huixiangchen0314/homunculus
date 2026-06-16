(ns top.kzre.homunculus.core.types.recur-elim.methods.let
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :let [node]
  (n/make-let (mapv (fn [[v e]] [(rec/eliminate v) (rec/eliminate e)]) (n/let-bindings node))
              (rec/eliminate (n/let-body node))
              (n/attrs node) (n/node-meta node) (n/parent node)))
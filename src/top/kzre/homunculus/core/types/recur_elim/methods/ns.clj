;; recur_elim/methods/ns.clj
(ns top.kzre.homunculus.core.types.recur-elim.methods.ns
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :ns [node]
  ;; NsNode 没有子节点，无需递归，直接返回
  node)
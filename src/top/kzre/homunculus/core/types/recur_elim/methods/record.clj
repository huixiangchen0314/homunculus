;; recur_elim/methods/record.clj
(ns top.kzre.homunculus.core.types.recur-elim.methods.record
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :record [node]
  ;; RecordNode 无子节点，直接返回
  node)
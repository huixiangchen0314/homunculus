(ns top.kzre.homunculus.core.types.recur-elim.methods.try
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :try [node]
  (n/make-try (rec/eliminate (n/try-body node))            ;; body 为单个节点
              (mapv rec/eliminate (n/try-catches node))    ;; catches 为列表
              (when-let [f (n/try-finally node)]
                (rec/eliminate f))                          ;; finally 为单个节点或 nil
              (n/attrs node) (n/node-meta node) (n/parent node)))
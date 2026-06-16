(ns top.kzre.homunculus.core.ir2.forms.define
  (:require [top.kzre.homunculus.core.ir1.node :as n1]
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :def [node env]
  (let [name-sym (n1/sym-name (n1/def-name node))   ;; 从 SymbolNode 中取出符号
        val-node (when-let [v (n1/def-val node)]
                   (first (ir2/lower-ast v env)))]
    [(n2/make-define name-sym val-node nil    ;; doc 暂忽略
                     {}                       ;; attr 暂忽略
                     (n1/node-meta node) nil)]))
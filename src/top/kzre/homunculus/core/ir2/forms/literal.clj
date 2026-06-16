(ns top.kzre.homunculus.core.ir2.forms.literal
  (:require [top.kzre.homunculus.core.ir1.node :as n1]   ;; IR1 节点访问器
            [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.node :as n2]))

(defmethod ir2/lower-ast :literal [node env]
  [(n2/make-literal (n1/lit-val node)                     ;; 使用 ir1.node 访问器
                    {}
                    (n1/node-meta node)                   ;; IR1 元数据 -> IR2
                    nil)])
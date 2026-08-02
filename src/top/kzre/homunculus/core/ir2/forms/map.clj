(ns top.kzre.homunculus.core.ir2.forms.map
  (:require [top.kzre.homunculus.core.ir2.core :as ir2]
            [top.kzre.homunculus.core.ir2.model :as model]))

(defmethod ir2/lower-ast :map [node env]
  (let [ir1-pairs (:pairs node)                    ;; 现在是一组 Pair 节点
        ir2-pairs (mapv #(first (ir2/lower-ast % env)) ir1-pairs)]
    [(model/->Map ir2-pairs {} (:meta node)) env]))   ;; 显式调用构造器
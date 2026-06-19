(ns top.kzre.homunculus.core.types.constraint.gen.methods.ns
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :ns [node context]
  ;; 命名空间声明不参与类型推导，分配一个类型变量
  (let [tv (gen/fresh-tvar)]
    [tv (ty/set-type! node tv) nil context]))
(ns top.kzre.homunculus.core.types.constraint.gen.methods.record
  (:require [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
            [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.type :as ty]))

(defmethod gen/cg-node-raw :record [node context]
  ;; 记录声明不参与类型推导
  (let [tv (gen/fresh-tvar)]
    [tv (ty/set-type! node tv) nil]))
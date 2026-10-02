(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.ns
  (:require
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod gen/gen-node* :ns [current-node context]
  ;; 命名空间声明不参与类型推导，分配一个类型变量
  (let [tv (gen/fresh-tvar)]
    {:type        tv
     :node        (ty/set-type! current-node tv)
     :constraints nil
     :env         context}))
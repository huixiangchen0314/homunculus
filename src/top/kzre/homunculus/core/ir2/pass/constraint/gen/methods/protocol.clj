(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.methods.protocol
  (:require
    [top.kzre.homunculus.core.ir2.pass.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defmethod gen/gen-node* :protocol [current-node env]
  ;; 协议声明不参与类型推导
  (let [tv (gen/fresh-tvar)]
    {:type        tv
     :node        (ty/set-type! current-node tv)
     :constraints nil
     :env         env}))
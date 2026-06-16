(ns top.kzre.homunculus.core.types.inline-lift.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :lambda [node config lifted]
  (m/->LambdaNode (mapv #(walk % config lifted) (:params node))
                  (walk (:body node) config lifted)
                  (:captures node) (:fn-name node)
                  (:attrs node) (:meta node) (:parent node)))
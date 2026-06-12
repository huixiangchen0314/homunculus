(ns top.kzre.homunculus.core.types.recur-elim.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :lambda [node]
  (m/->LambdaNode (mapv eliminate (:params node))
                  (eliminate (:body node))
                  (:captures node) (:fn-name node)
                  (:attrs node) (:meta node) (:parent node)))
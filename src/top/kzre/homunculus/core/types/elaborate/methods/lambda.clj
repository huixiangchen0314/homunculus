(ns top.kzre.homunculus.core.types.elaborate.methods.lambda
  (:require [top.kzre.homunculus.core.ir2.model :as m]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :lambda [node ir2-roots config new-defs]
  (m/->LambdaNode (mapv #(eliminate % ir2-roots config new-defs) (:params node))
                  (eliminate (:body node) ir2-roots config new-defs)
                  (:captures node) (:fn-name node)
                  (:attrs node) (:meta node) (:parent node)))
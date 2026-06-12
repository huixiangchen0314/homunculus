(ns top.kzre.homunculus.core.types.elaborate.methods.default
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.elaborate.core :refer :all]))

(defmethod eliminate :default [node ir2-roots config new-defs]
  (throw (ex-info (str "Unknown node kind in elaborate: " (ir2p/kind node))
                  {:node node})))
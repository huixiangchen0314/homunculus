(ns top.kzre.homunculus.core.types.recur-elim.methods.default
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.recur-elim.core :refer :all]))

(defmethod eliminate :default [node]
  (throw (ex-info (str "Unknown node kind in recur-elim: " (ir2p/kind node))
                  {:node node})))
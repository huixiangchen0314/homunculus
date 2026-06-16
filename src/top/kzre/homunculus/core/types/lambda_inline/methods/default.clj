(ns top.kzre.homunculus.core.types.inline-lift.methods.default
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.inline-lift.core :refer :all]))

(defmethod walk :default [node config lifted]
  (throw (ex-info (str "Unknown node kind in inline-lift: " (ir2p/kind node))
                  {:node node})))
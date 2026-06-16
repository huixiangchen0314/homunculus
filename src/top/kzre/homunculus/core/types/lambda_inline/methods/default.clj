(ns top.kzre.homunculus.core.types.lambda-inline.methods.default
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.lambda-inline.core :as inline]))

(defmethod inline/eliminate-inline :default [node _config]
  (throw (ex-info (str "Unknown node kind in inline: " (ir2p/kind node))
                  {:node node})))
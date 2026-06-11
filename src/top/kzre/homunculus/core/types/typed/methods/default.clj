(ns top.kzre.homunculus.core.types.typed.methods.default
  (:require [top.kzre.homunculus.core.types.typed.core :as infer]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod infer/infer :default [node context]
  (throw (ex-info (str "Type inference not implemented for " (ir2p/kind node))
                  {:node node})))
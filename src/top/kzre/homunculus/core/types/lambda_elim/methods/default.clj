(ns top.kzre.homunculus.core.types.lambda-elim.methods.default
  (:require [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :default [node _ir2-roots _config _new-defs]
  (throw (ex-info (str "Unknown node kind in lambda-elim: " (ir2p/kind node))
                  {:node node})))
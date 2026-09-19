(ns top.kzre.homunculus.core.ir2.pass.check.methods.default
  (:require [top.kzre.homunculus.core.ir2.pass.check.core :as check]))

(defmethod check/check-node :default [node expected context]
  (throw (ex-info (str "Check not implemented for " (get-in node [:kind]))
                  {:node node})))
(ns top.kzre.homunculus.core.types.check.methods.default
  (:require [top.kzre.homunculus.core.types.check.core :as check]))

(defmethod check/check :default [node expected context]
  ;; 未知节点，抛异常
  (throw (ex-info (str "Check not implemented for " (get-in node [:kind]))
                  {:node node})))
(ns top.kzre.homunculus.core.types.lambda-elim.methods.variable
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :variable [node _roots _config _defs]
  node)
(ns top.kzre.homunculus.core.types.lambda-elim.methods.literal
  (:require [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :literal [node config env]
  [node []])
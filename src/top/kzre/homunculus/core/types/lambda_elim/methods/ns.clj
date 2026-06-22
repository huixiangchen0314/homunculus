(ns top.kzre.homunculus.core.types.lambda-elim.methods.ns
  (:require [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :ns [node _config _env]
  [node []])
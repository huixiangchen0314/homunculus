(ns top.kzre.homunculus.core.types.lambda-elim.methods.variable
  (:require [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :variable [node _config _env]
  [node []])
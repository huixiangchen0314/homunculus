(ns top.kzre.homunculus.core.types.lambda-elim.methods.protocol
  (:require [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :protocol [node _config _env]
  [node []])
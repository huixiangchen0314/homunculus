(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
   [top.kzre.homunculus.backend.hlsl.core :as core]
   [top.kzre.homunculus.internal.protocol :as p]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ unit context]
    (core/emit unit (core/make-context context (p/frontend context)))))

(defonce emitter (->HLSLEmitter))
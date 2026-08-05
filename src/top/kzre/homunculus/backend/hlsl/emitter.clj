(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
   [top.kzre.homunculus.backend.hlsl.core :as core]
   [top.kzre.homunculus.backend.shader.lower :as lower]
   [ top.kzre.homunculus.backend.shader.statementize :as stmt]
   [top.kzre.homunculus.internal.protocol :as p]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ nodes context]
    (println "ir2: " nodes)
    ;(println "lowered: " (lower/lower-nodes nodes))
    (let [lowered (lower/lower-nodes nodes)
          stmted (stmt/statementize-nodes lowered)]
      (println "lowered" lowered)
      (println "statementized" stmted))
    (core/emit nodes (core/make-context context (p/frontend context)))))

(defonce emitter (->HLSLEmitter))
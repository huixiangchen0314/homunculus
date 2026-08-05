(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
   [top.kzre.homunculus.backend.hlsl.core :as core]
   [top.kzre.homunculus.backend.shader.lower :as lower]
   [ top.kzre.homunculus.backend.shader.statementize :as stmt]
   [top.kzre.homunculus.backend.hlsl.emit :as emit]
   [top.kzre.homunculus.internal.protocol :as p]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ nodes context]
    (println "ir2: " nodes)
    ;(println "lowered: " (lower/lower-nodes nodes))
    (let [lowered (lower/lower-nodes nodes)
          stmted (stmt/statementize-nodes lowered)
          emitted (emit/emit-nodes stmted)]
      (println "lowered" lowered)
      (println "statementized" stmted)
      (print "emitted:" emitted))
    (core/emit nodes (core/make-context context (p/frontend context)))))

(defonce emitter (->HLSLEmitter))
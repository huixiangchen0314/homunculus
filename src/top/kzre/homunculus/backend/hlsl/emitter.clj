(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
   [ top.kzre.homunculus.backend.shader.statementize :as stmt]
   [top.kzre.homunculus.backend.hlsl.emit :as emit]
   [top.kzre.homunculus.backend.shader.lower :as lower]
   [top.kzre.homunculus.backend.shader.vec-assign :as vec-assign]
   [top.kzre.homunculus.internal.protocol :as p]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ nodes context]
    (println "ir2: " nodes)
    (let [lowered (lower/lower-nodes nodes)
          stmted (stmt/statementize-nodes lowered)
          vec-copied (vec-assign/expand-vec-assigns stmted)
          emitted (emit/emit-nodes vec-copied)]
      (println "lowered" lowered)
      (println "statementized" stmted)
      (print "emitted:" emitted)
      emitted)
    ;(core/emit nodes (core/make-context context (p/frontend context)))
    ))

(defonce emitter (->HLSLEmitter))
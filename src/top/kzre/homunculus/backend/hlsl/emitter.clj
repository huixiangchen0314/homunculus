(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
   [ top.kzre.homunculus.backend.shader.statementize :as stmt]
   [top.kzre.homunculus.backend.hlsl.emit :as emit]
   [top.kzre.homunculus.backend.shader.assign-propagate :as assign-propagate]
   [top.kzre.homunculus.backend.shader.lower :as lower]
   [top.kzre.homunculus.backend.shader.vec-assign :as vec-assign]
   [top.kzre.homunculus.core.irstmt.lower :as irstmt.lower]
   [top.kzre.homunculus.internal.protocol :as p]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ nodes context]
    (println "ir2: " nodes)
    (let [irstmts (irstmt.lower/lower-nodes nodes)
          shader-asts (lower/lower-nodes nodes)
          stmted (stmt/statementize-nodes shader-asts)
          proped (assign-propagate/propagate-nodes stmted)
          vec-copied (vec-assign/expand-vec-assigns proped)
          emitted (emit/emit-nodes vec-copied)]
      (println "shader-asts" shader-asts)
      (println "statementized" stmted)
      (print "emitted:" emitted)
      emitted)
    ))

(defonce emitter (->HLSLEmitter))
(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
    [top.kzre.homunculus.backend.hlsl.emit :as emit]
    [top.kzre.homunculus.backend.shader.lower :as lower]
    [top.kzre.homunculus.backend.shader.vec-assign :as vec-assign]
   [top.kzre.homunculus.core.irstmt.assign-propagate :as irstmt.assign-propagate]
   [top.kzre.homunculus.core.irstmt.lower :as irstmt.lower]
   [top.kzre.homunculus.core.irstmt.statementize :as statementize]
   [top.kzre.homunculus.internal.protocol :as p]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ nodes context]
    (println "ir2: " nodes)
    (let [irstmts (irstmt.lower/lower-nodes nodes)
          sted (statementize/statementize-nodes irstmts)
          ped (irstmt.assign-propagate/propagate-nodes sted)
          shader-asts (lower/lower-nodes ped)
          vec-copied (vec-assign/expand-vec-assigns shader-asts)
          emitted (emit/emit-nodes vec-copied)]
      (println "ir-stmts " irstmts)
      (println "stmted" sted)
      (println "shader-asts" shader-asts)
      (print "emitted:" emitted)
      emitted)
    ))

(defonce emitter (->HLSLEmitter))
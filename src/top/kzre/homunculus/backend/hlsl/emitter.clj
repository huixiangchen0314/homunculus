(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
    [top.kzre.homunculus.backend.hlsl.emit :as emit]
    [top.kzre.homunculus.backend.shader.lower :as lower]
    [top.kzre.homunculus.backend.shader.vec-assign :as vec-assign]
    [top.kzre.homunculus.core.irstmt.assign-propagate :as irstmt.assign-propagate]
    [top.kzre.homunculus.core.irstmt.dce :as irstmt.dce]
    [top.kzre.homunculus.core.irstmt.lower :as irstmt.lower]
    [top.kzre.homunculus.core.irstmt.statementize :as statementize]
    [top.kzre.homunculus.internal.module-unit :as mu]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.model :as m]
    [top.kzre.homunculus.internal.utils :as iu]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ nodes ctx {:keys [unit]}]
    (println "ir2: " nodes)
    (let [config (p/config ctx)
          out-dir (p/output-dir (p/config ctx))
          path (if unit (str out-dir "/" (iu/ns->module-path (mu/module-ns unit)
                                                             (p/module-naming-style config)
                                                             ".hlsl"))
                        out-dir)
          irstmts (irstmt.lower/lower-nodes nodes)
          sted (statementize/statementize-nodes irstmts)
          ped (irstmt.assign-propagate/propagate-nodes sted)
          dced (irstmt.dce/elim-nodes ped ctx)
          shader-asts (lower/lower-nodes dced)
          vec-copied (vec-assign/expand-vec-assigns shader-asts)
          emitted (emit/emit-nodes vec-copied)]
      (println "ir-stmts " irstmts)
      (println "stmted" sted)
      (println "shader-asts" shader-asts)
      (print "emitted:" emitted)
      (m/make-module-result path emitted))
    ))

(defonce emitter (->HLSLEmitter))
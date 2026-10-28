(ns top.kzre.homunculus.backend.hlsl.emitter
  (:require
    [top.kzre.homunculus.backend.format :as format]
    [top.kzre.homunculus.backend.hlsl.emit :as emit]
    [top.kzre.homunculus.backend.shader.lower :as lower]
    [top.kzre.homunculus.backend.shader.pass.array-assign :as array-assign]
    [top.kzre.homunculus.core.cfg.irstmt.adapt :as cfg-adapt]
    [top.kzre.homunculus.core.cfg.irstmt.opt :as opt]
    [top.kzre.homunculus.core.irstmt.lower :as irstmt.lower]
    [top.kzre.homunculus.core.irstmt.pass.statementize :as statementize]
    [top.kzre.homunculus.internal.compile-result :as compile-result]
    [top.kzre.homunculus.internal.module-unit :as mu]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.utils :as iu]))


(defrecord HLSLEmitter []
  p/IEmitter
  (emit [_ nodes ctx {:keys [unit]}]
    (println "ir2: " nodes)
    (let [config (p/config ctx)
          out-dir (p/output-path (p/config ctx))
          path (format/ensure-suffix
                 (if unit
                   (str out-dir "/"
                        (iu/ns->module-path (mu/module-ns unit)
                                            (p/module-naming-style config)))
                          out-dir)
                 ".hlsl")
          irstmts (irstmt.lower/lower nodes (irstmt.lower/make-env))
          sted (statementize/statementize-nodes irstmts)
          cfged        (opt/optimize sted (cfg-adapt/make-adapter ctx))
          shader-asts (lower/lower cfged)
          vec-copied (array-assign/expand shader-asts)
          emitted (emit/emit vec-copied)]


      (println "ir-stmts " irstmts)
      (println "stmted" sted)
      (println "shader-asts" shader-asts)
      (print "emitted:" emitted)
      (compile-result/make-module-result path emitted))))

(defonce ^:private emitter* (->HLSLEmitter))

(defn emitter [] emitter*)
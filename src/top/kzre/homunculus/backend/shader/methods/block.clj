(ns top.kzre.homunculus.backend.shader.methods.block
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]
            [clojure.string :as str]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]))

(defmethod emit :block [node backend]
  (let [stmts (:exprs node)
        n (count stmts)
        emitted (map-indexed (fn [i expr]
                               (let [code (emit expr backend)]
                                 (if (and (= i (dec n))
                                          (satisfies? ir2p/INode expr)
                                          (#{:literal :call :variable :vector} (ir2p/kind expr)))
                                   (sp/shader-return backend code)
                                   code)))
                             stmts)]
    (str/join ";\n" emitted)))
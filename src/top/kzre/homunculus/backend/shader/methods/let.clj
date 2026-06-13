(ns top.kzre.homunculus.backend.shader.methods.let
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.string :as str]))

(defmethod emit :let [node backend]
  (let [bindings (:bindings node)
        body     (:body node)
        var-decls (map (fn [[var val]]
                         (let [var-name (:name var)
                               var-type (get-in var [:attrs :type])
                               mutable? (get-in var [:attrs :mutable])
                               val-code (emit val backend)]
                           (sp/shader-var-decl backend var-name var-type (boolean mutable?) val-code)))
                       bindings)
        body-code (emit body backend)
        body-final (if (or (not (satisfies? ir2p/INode body))
                           (#{:literal :call :variable :vector} (ir2p/kind body)))
                     (sp/shader-return backend body-code)
                     body-code)]
    (str (str/join "\n" var-decls) "\n" body-final)))
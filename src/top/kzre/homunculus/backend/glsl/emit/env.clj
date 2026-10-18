(ns top.kzre.homunculus.backend.glsl.emit.env
  (:require
    [top.kzre.homunculus.backend.glsl.options :as opt]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IEnv
  (gl-version [_])
  (ns-strategy [_]))

(defrecord Env [compile-ctx options]
  IEnv
  (gl-version [_]
    (opt/gl-version options))
  (ns-strategy [_]
    (opt/ns-strategy options)))

(defn make-env [compile-ctx]
  (let [options (ip/options (ip/config compile-ctx))]
    (->Env compile-ctx options)))
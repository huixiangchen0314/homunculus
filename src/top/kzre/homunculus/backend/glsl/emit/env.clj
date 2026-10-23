(ns top.kzre.homunculus.backend.glsl.emit.env
  (:require
   [top.kzre.homunculus.backend.glsl.config :as cfg]
   [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IEnv
  (gl-version [_])
  (ns-strategy [_]))

(defrecord Env [compile-ctx config]
  IEnv
  (gl-version [_]
    (cfg/gl-version config))
  (ns-strategy [_]
    (cfg/ns-strategy config)))

(defn make-env [compile-ctx]
  (->Env compile-ctx (ip/config compile-ctx) ))
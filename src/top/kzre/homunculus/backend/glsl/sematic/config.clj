(ns top.kzre.homunculus.backend.glsl.sematic.config
  (:require
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IConfig
  (gl-version [_] "目标 gl 版本 :330 :430 :440"))

(defrecord Config [compile-ctx options]
  IConfig
  (gl-version [_] (:gl-version options :330)))

(defn make-config [compile-ctx]
  (let [options (ip/options compile-ctx)]
    (->Config compile-ctx options)))

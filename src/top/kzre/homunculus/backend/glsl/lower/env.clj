(ns top.kzre.homunculus.backend.glsl.lower.env
  (:require
    [top.kzre.homunculus.internal.protocol :as proto]))


(defprotocol IEnv
  (low-entry-strategy [_]
    "入口点降级策略
    :wrap 生成包装函数，如果原函数叫做main就报错
    :inplace 原地改造原函数，不生成包装"))

(defrecord Env [compile-ctx options]
  IEnv
  (low-entry-strategy [_]
    (:gl-low-entry-strategy options :wrap)))

(defn make-env [compile-ctx ]
  (let [opts (proto/config compile-ctx)]
    (->Env compile-ctx opts)))

(ns top.kzre.homunculus.backend.hlsl.backend
  "HLSL 后端实现，提供类型转换规则。"
  (:require
   [top.kzre.homunculus.backend.hlsl.cast :as cast]
   [top.kzre.homunculus.backend.shader.folder :as folder]
   [top.kzre.homunculus.core.ir2.pass.protocol :as tp]))

(defrecord HLSLBackend []
  tp/IBackendInfo
  (type-conversion [_ src-ty dst-ty]
    ;; HLSL 隐式转换规则，返回代价（nil 表示不允许）
    (cast/conversion-cost src-ty dst-ty false))
  (support-hetero-vec [_] false)
  (folder [_] (folder/folder))
  (mutable-vector? [_] true)
  (vector-copy-assign? [_] true)
  (copy-on-assign? [_] true))


(defonce ^:private backend* (->HLSLBackend))

(defn backend [] backend*)
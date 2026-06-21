
(ns top.kzre.homunculus.examples.hlsl-lambert.core
  (:require [top.kzre.homunculus.backend.shader.dsl :refer :all]
            [my.shaders.fog]))

;; ── 资源声明 ──────────────────────────────
(deftexture myTexture :t0)
(defsampler mySampler :s0)


(defcbuffer LightParams :b0
            lightDir float3
            lightColor float4
            ambient float4)

(defuniform worldViewProj float4x4)

(defstatic accumColor (float4 0.0 0.0 0.0 0.0))

(defn remin-sum [^float a] a)



(defrecord MyInout [^:SV_TARGET ^float a 0.0])


;(defrecord MainOutput [^:float4 color])

(defshader :vertex vsMain
           [^:POSITION ^float4 pos
            ^:NORMAL ^float3 nrm
            ^:TEXCOORD0 ^float2 uv]
           (let [worldPos (mul worldViewProj pos)]
             (float4 (float3 1.0 1.0 1.0) 1.0)))

(defshader :fragment psMain
           [^:SV_POSITION ^float4 pos
            ^:NORMAL ^float3 nrm
            ^:TEXCOORD0 ^float2 uv]
           (let [diffuse (sample myTexture mySampler uv)
                 N (normalize nrm)
                 L (normalize lightDir)
                 diff (max 0 (dot N L))
                 color (* diffuse (* lightColor diff))
                 finalColor (+ color ambient)]
             finalColor))
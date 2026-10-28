(ns top.kzre.homunculus.examples.hlsl-lambert.core
  (:require-macros
    [top.kzre.homunculus.backend.shader.dsl :refer :all]))

;; ── 资源声明 ──────────────────────────────
(deftexture myTexture :t0)
(defsampler mySampler :s0)
(deftexture uIndexTable :t0 :buffer uint)

(defcbuffer LightParams :b0
            lightDir float3
            lightColor float4
            ambient float4)

(defuniform worldViewProj float4x4)

(defstatic accumColor (float4 0.0 0.0 0.0 0.0))

(defn riemann-integral [a] (- a 1.0))

(defrecord VertOutput [^:position ^float4 pos
                       ^:normal ^float3 nrm
                       ^:texcoord0 ^float2 uv])

;; ── 顶点着色器 (包含高阶调用测试) ────────
(defshader :vertex vsMain
           [^:position ^float4 pos
            ^:normal ^float3 nrm
            ^:texcoord0 ^float2 uv]
           (let [worldPos (mul worldViewProj pos)
                 ll (if true (if false  2 3) 1 )
                 xxxxx [1 2 3]]
             (->VertOutput (float4 (float3 uv 1.0) (%%aget xxxxx 0))
                           (float3 0.0 0.0 ll)
                           (float2 0.0 0.0))))

;; ── 片段着色器 ────────────────────────────
(defshader :fragment psMain
           ^:target0
           [^:position ^float4 pos
            ^:normal ^float3 nrm
            ^:texcoord0 ^float2 uv]
           (let [lv (load uIndexTable 1)
                 diffuse (sample myTexture mySampler uv)
                 N (normalize nrm)
                 L (normalize lightDir)
                 diff (max 0 (dot N L))
                 color (* diffuse (* lightColor diff))
                 finalColor (+ color ambient)]
                finalColor))
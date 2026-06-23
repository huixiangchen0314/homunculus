(ns top.kzre.homunculus.examples.hlsl-lambert.core
  (:require [top.kzre.homunculus.backend.shader.dsl :refer :all] ;; 不支持对编译模块用 refer all
            [top.kzre.homunculus.examples.hlsl-lambert.lib :as l]))

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


;; ── 顶点着色器 (包含高阶调用测试) ────────
(defshader :vertex vsMain
           [^:POSITION ^float4 pos
            ^:NORMAL ^float3 nrm
            ^:TEXCOORD0 ^float2 uv]
           (def x (%%new-array 3))
           (%%aset x 0 0)
           (%%aset x 1 1)
           (%%aset x 2 2)
           ;; 使用 my-map 对 x 的每个元素加 1
           (def y (l/my-map (fn [v] (+ v 1)) x))
           (let [worldPos (mul worldViewProj pos)]
             (float4 (float3 1.0 1.0 1.0) 1.0)))

;; ── 片段着色器 ────────────────────────────
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
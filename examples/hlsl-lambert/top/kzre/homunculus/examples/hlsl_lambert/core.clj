(ns top.kzre.homunculus.examples.hlsl-lambert.core
  (:require [top.kzre.homunculus.backend.shader.dsl :refer :all]
            [top.kzre.homunculus.examples.hlsl-lambert.lib :as lib]))

;; ── 资源声明 ──────────────────────────────
(deftexture myTexture :t0)
(defsampler mySampler :s0)

(defcbuffer LightParams :b0
            lightDir float3
            lightColor float4
            ambient float4)

(defuniform worldViewProj float4x4)

(defstatic accumColor (float4 0.0 0.0 0.0 0.0))

(defn remin-sum [a] (- a 1))

(defrecord MyInout [^:SV_TARGET ^float a])


;; ── 顶点着色器 (包含高阶调用测试) ────────
(defshader :vertex vsMain
           [^:POSITION ^float4 pos
            ^:NORMAL ^float3 nrm
            ^:TEXCOORD0 ^float2 uv
            ]
           (def x (%%new-array 3))

           (%%aset x 0 0)
           (%%aset x 1 1)
           (%%aset x 2 2)
           (def svsv (remin-sum (%%aget x 1)))
           (def sum-x (reduce + 0 x))
           (def sv (conj x 6))
           (def vv 6)
           (set! vv 3)
           (def avv (%%new-array vv))
           (%%aset avv 1 1.0)
           ;; 使用 my-map 对 x 的每个元素加 1
           (def y (map (fn [v] (+ v 1)) x))
           (def io (->MyInout 1.0))
           (def local-a (:a io))
           (let [worldPos (mul worldViewProj pos)
                 ll (if true 1 2)
                 xxxxx [1 2 3]]
             (float4 (float3 ll 1.0 1.0) 1.0)))

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
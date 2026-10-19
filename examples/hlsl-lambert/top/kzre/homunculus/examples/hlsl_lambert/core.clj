(ns top.kzre.homunculus.examples.hlsl-lambert.core
  (:require
    [top.kzre.homunculus.examples.hlsl-lambert.lib :as lib]
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
            ^:texcoord0 ^float2 uv
            ]
           (def x (%%new-array 3))

           (%%aset x 0 1)
           (%%aset x 1 1)
           (%%aset x 2 2)
           (def svsv (riemann-integral (%%aget x 1)))
           (def sum-x (reduce + 0 x))
           (def sv (conj x 6))
           (%%aset sv 3 0)
           (def vv 6)
           (set! vv 3)
           (def avv (%%new-array vv))
           ;; 使用 my-map 对 x 的每个元素加 1
           (def y (map (fn [v] (* (+ v 1.0) 2)) sv))
           (let [worldPos (mul worldViewProj pos)
                 ll (if true 1 2)
                 xxxxx [1 2 3]]
             (->VertOutput (float4 (float3 (%%aget y 0) vv 1.0) (%%aget xxxxx 0))
                           (float3 0.0 0.0 0.0)
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
                (def ^{:aaa MyInout} f (range 10))
                (def g (drop 3 f))
                (def z (butlast g))
                finalColor))
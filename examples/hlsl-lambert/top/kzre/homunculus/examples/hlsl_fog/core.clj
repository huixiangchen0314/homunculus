(ns top.kzre.homunculus.examples.hlsl-fog.core
  (:require [top.kzre.homunculus.backend.shader.dsl :refer :all]
            [my.shaders.fog]))          ;; 可选的雾函数库，本次未使用

;; ── 体积雾参数（通过 uniform 暴露）──────
(defuniform cameraPos   float3)          ;; 世界空间相机位置
(defuniform fogColor    float4)          ;; 雾的颜色
(defuniform fogDensity  float)           ;; 基础雾密度系数
(defuniform stepCount   int)             ;; 光线步进次数

;; ── 标准资源 ──────────────────────────
(deftexture myTexture :t0)
(defsampler mySampler :s0)

(defcbuffer LightParams :b0
            lightDir float3
            lightColor float4
            ambient float4)

(defuniform worldViewProj float4x4)

;; ── 静态变量与记录 ────────────────────
(defstatic accumColor (float4 0.0 0.0 0.0 0.0))
(defrecord MyInout [^:SV_TARGET ^float a 0.0])

;; ── 辅助函数：雾密度随高度指数衰减 ────
(defn fog-density [worldPos]
  (let [height (y worldPos)]
    (* fogDensity (exp (- 0.0 height)))))   ;; exp(-height)

;; ── 顶点着色器 ────────────────────────
(defshader :vertex vsMain
           [^:POSITION ^float4 pos
            ^:NORMAL ^float3 nrm
            ^:TEXCOORD0 ^float2 uv]
           (let [worldPos (mul worldViewProj pos)]
             (float4 (float3 1.0 1.0 1.0) 1.0)))    ;; 简化，返回裁剪位置

;; ── 片段着色器（体积雾 + Lambert）─────
(defshader :fragment psMain
           [^:SV_POSITION ^float4 pos
            ^:NORMAL ^float3 nrm
            ^:TEXCOORD0 ^float2 uv]
           (let [;; 世界空间位置（此处简化为传入的pos，实际需逆变换）
                 worldPos (float3 (:x pos) (:y pos) (:z pos))
                 ;; 射线方向：从相机指向片段
                 rayDir   (normalize (- worldPos cameraPos))
                 ;; 步长
                 stepSize (/ (length (- worldPos cameraPos)) (float stepCount))

                 ;; ── 使用 map/reduce 计算黎曼和 ──
                 ;; 生成步进索引序列 0..stepCount-1
                 indices  (range stepCount)
                 ;; 对每个索引计算密度贡献
                 densities (map (fn [i]
                                  (let [t (* (float i) stepSize)
                                        samplePos (+ cameraPos (* t rayDir))]
                                    (* stepSize (fog-density samplePos))))
                                indices)
                 ;; 总光学深度
                 totalDensity (reduce + densities)

                 ;; 透射率
                 transmittance (exp (- totalDensity))

                 ;; ── 标准 Lambert 光照 ──
                 N (normalize nrm)
                 L (normalize lightDir)
                 diff (max 0.0 (dot N L))
                 litColor (* (sample myTexture mySampler uv)
                             (+ ambient (* lightColor diff)))
                 ;; 混合雾色
                 finalColor (+ (* litColor transmittance)
                               (* fogColor (- 1.0 transmittance)))]
             finalColor))
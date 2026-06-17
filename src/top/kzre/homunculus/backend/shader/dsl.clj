;; top.kzre.homunculus.backend.shader.dsl
(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。提供统一的资源/入口声明，通过元数据传递给后端。"
  (:refer-clojure :exclude [float]))

(defmacro defshader
  "定义着色器入口函数。stage 为 :vertex, :fragment 等，params 为带类型标注的参数向量。"
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn* params body)
                 {:shader-stage stage
                  :shader/entry? true})))

;; 类型构造器，用于标记类型
(defn- texture2D  [] nil)
(defn- sampler-state [] nil)
(defn- cbuffer [] nil)
(defn float [] nil)
(defn float2 [] nil)
(defn float3 [] nil)
(defn float4 [] nil)
(defn float4x4 [] nil)

(defmacro defuniform
  "定义全局 uniform 常量。示例：(defuniform worldViewProj float4x4)"
  [name type-ctor]
  `(def ~(vary-meta name assoc :shader/uniform? true )
     (~type-ctor)))



(defmacro deftexture
  "定义纹理资源。"
  [name register-kw]
  `(def ~(vary-meta name assoc
                    :shader/resource? true
                    :shader/resource-kind :texture2D
                    :shader/texture-register register-kw)
     (texture2D)))

(defmacro defsampler
  "定义采样器资源。"
  [name register-kw]
  `(def ~(vary-meta name assoc
                    :shader/resource? true
                    :shader/resource-kind :sampler
                    :shader/sampler-register register-kw)
     (sampler-state )))

(defmacro defcbuffer
  "定义 cbuffer 资源。成员以交替的符号+类型构造器给出，如 lightDir float3。
   示例：(defcbuffer LightParams :b0 lightDir float3 lightColor float4 ambient float4)"
  [name register-kw & members]
  (let [pairs (partition 2 members)
        map-expr (into {} (map (fn [[sym type-ctor]]
                                 [(keyword sym) type-ctor])
                               pairs))]
    `(def ~(vary-meta name assoc
                      :shader/resource? true
                      :shader/resource-kind :cbuffer
                      :shader/cbuffer-register register-kw
                      :shader/cbuffer-members map-expr)
       (cbuffer))))
(ns top.kzre.homunculus.backend.hlsl.utils
  "HLSL 专用工具：保留字、类型构造快捷函数。"
  (:require [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.typed.scheme :as sc]
            [top.kzre.homunculus.core.types.constraint.scheme :as scheme]))

(def reserved-words
  "HLSL 保留字集合，用于自动转义冲突的变量名。"
  #{"if" "else" "while" "for" "return" "true" "false"
    "float" "half" "double" "int" "uint" "bool" "void"
    "float2" "float3" "float4" "float3x3" "float4x4"
    "int2" "int3" "int4" "uint2" "uint3" "uint4"
    "matrix" "vector" "sampler" "sampler1D" "sampler2D"
    "sampler3D" "samplerCube" "Texture1D" "Texture2D"
    "Texture3D" "TextureCube" "RWTexture1D" "RWTexture2D"
    "RWTexture3D" "Buffer" "RWBuffer" "StructuredBuffer"
    "RWStructuredBuffer" "ByteAddressBuffer" "RWByteAddressBuffer"
    "cbuffer" "tbuffer" "register" "packoffset"
    "in" "out" "inout" "uniform" "static" "const"
    "discard" "compile" "namespace" "technique" "pass"
    "pixelshader" "vertexshader" "geometryshader" "hullshader"
    "domainshader" "computeshader"})

;; ── 类型构造快捷函数（用于 frontend builtins）──
(defn fn-> [& types]
  "从参数类型列表和返回类型构造多参数 TFun。"
  (let [ret (last types)
        args (butlast types)]
    (reduce (fn [acc arg] (t/->TFun arg acc)) ret (reverse args))))

(defn bin-op [ty ret]
  (fn-> ty ty ret))

(defn unary-math [ty]
  (fn-> ty ty))

(defn binary-math [ty]
  (fn-> ty ty ty))

(defn ternary-math [ty]
  (fn-> ty ty ty ty))

(defn vector-ctor [vec-ty n]
  (let [args (repeat n (t/->TCon :float))]
    (apply fn-> (concat args [vec-ty]))))

(defn generic-unary []
  (let [a (t/->TVar (gensym "a"))]
    (scheme/->TScheme [a] (fn-> a a))))

(defn generic-binary []
  (let [a (t/->TVar (gensym "a"))]
    (scheme/->TScheme [a] (fn-> a a a))))

(defn generic-ternary []
  (let [a (t/->TVar (gensym "a"))]
    (scheme/->TScheme [a] (fn-> a a a a))))


;; ═══ 新增：向量到标量的泛型函数 ═══
(defn generic-vector-to-float-unary []
  "∀ a. a -> float  （如 length）"
  (let [a (t/->TVar (gensym "a"))]
    (sc/->TScheme [a] (fn-> a (t/->TCon :float)))))

(defn generic-vector-to-float-binary []
  "∀ a. a -> a -> float （如 distance）"
  (let [a (t/->TVar (gensym "a"))]
    (sc/->TScheme [a] (fn-> a a (t/->TCon :float)))))

;; top.kzre.homunculus.backend.unity.frontend
(ns top.kzre.homunculus.backend.unity.frontend
  "Unity ShaderLab 内置管线前端。
   实现 IFrontendInfo，提供 Unity 常用类型和内置函数。"
  (:require
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.backend.shader.builtin :as builtin]))

(defrecord UnityFrontend []
  tp/IFrontendInfo
  (frontend-types [_]
    [:float :float2 :float3 :float4 :float4x4
     :half :half2 :half3 :half4
     :fixed :fixed2 :fixed3 :fixed4
     :bool :int
     :sampler2D :samplerCUBE :texture2D :texture3D :cbuffer])

  (literal->type [_ val]
    (cond
      (float? val)   (t/->TCon :float)
      (integer? val) (t/->TCon :int)
      (true? val)    (t/->TCon :bool)
      (false? val)   (t/->TCon :bool)
      (nil? val)     (t/->TCon :float)
      :else          (t/->TVar (gensym "lit"))))

  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (t/->TCon tag)
        (t/->TCon (keyword (name tag))))))

  (builtin-functions [_]
    (merge builtin/common-builtins
           '{;; Unity 特有内置函数
             tex2D                 (t/->TFun (t/->TCon :sampler2D) (t/->TFun (t/->TCon :float2) (t/->TCon :float4)))
             tex2Dlod              (t/->TFun (t/->TCon :sampler2D) (t/->TFun (t/->TCon :float4) (t/->TCon :float4)))
             texCUBE               (t/->TFun (t/->TCon :samplerCUBE) (t/->TFun (t/->TCon :float3) (t/->TCon :float4)))
             UnityObjectToClipPos  (t/->TFun (t/->TCon :float4) (t/->TCon :float4))
             UnityWorldSpaceViewDir (t/->TFun (t/->TCon :float3) (t/->TCon :float3))
             lerp                  (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float))))
             })))

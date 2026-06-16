(ns top.kzre.homunculus.backend.hlsl.frontend
  "HLSL 前端：实现 IFrontendInfo 协议，提供 HLSL 类型、字面量、内置函数。"
  (:require
    [top.kzre.homunculus.backend.shader.builtin :as builtin]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.core.types.protocol :as tp]))

(defrecord HLSLFrontend []
  tp/IFrontendInfo
  (frontend-types [_]
    [:float :float2 :float3 :float4 :float4x4 :bool :int :texture2D :sampler :cbuffer])

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

  ;; 返回 HLSL 内置函数表，融合通用 shader builtins 和 HLSL 特有函数
  (builtin-functions [_]
    (merge builtin/common-builtins
           '{;; HLSL 特有内置函数
             tex2D           (t/->TFun (t/->TCon :sampler) (t/->TFun (t/->TCon :float2) (t/->TCon :float4)))
             tex2Dlod        (t/->TFun (t/->TCon :sampler) (t/->TFun (t/->TCon :float4) (t/->TCon :float4)))
             texCUBE         (t/->TFun (t/->TCon :sampler) (t/->TFun (t/->TCon :float3) (t/->TCon :float4)))
             clip            (t/->TFun (t/->TCon :float) (t/->TCon :void))
             discard         (t/->TFun (t/->TCon :void))
             ddx             (t/->TFun (t/->TCon :float) (t/->TCon :float))
             ddy             (t/->TFun (t/->TCon :float) (t/->TCon :float))
             fwidth          (t/->TFun (t/->TCon :float) (t/->TCon :float))
             lerp            (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float))))})))

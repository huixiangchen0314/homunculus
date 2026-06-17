(ns top.kzre.homunculus.backend.hlsl.frontend
  "HLSL 前端：实现 IFrontendInfo 协议，提供 HLSL 类型、字面量、内置函数。"
  (:require
    [top.kzre.homunculus.backend.shader.builtin :as builtin]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.core.types.type :as ty]))

(defrecord HLSLFrontend []
  tp/IFrontendInfo
  (frontend-types [_]
    [:float :float2 :float3 :float4 :float4x4 :bool :int :texture2D :sampler :cbuffer])

  (literal->type [_ val]
    (cond
      (float? val)   (ty/make-tcon :float)
      (integer? val) (ty/make-tcon :int)
      (true? val)    (ty/make-tcon :bool)
      (false? val)   (ty/make-tcon :bool)
      (nil? val)     (ty/make-tcon :float)
      :else          (ty/make-tvar (gensym "lit"))))

  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (ty/make-tcon tag)
        (ty/make-tcon (keyword (name tag))))))

  ;; 返回 HLSL 内置函数表，融合通用 shader builtins 和 HLSL 特有函数
  (builtin-functions [_]
    (merge builtin/common-builtins
           {;; HLSL 特有内置函数
            'tex2D           (ty/make-tfun (ty/make-tcon :sampler) (ty/make-tfun (ty/make-tcon :float2) (ty/make-tcon :float4)))
            'tex2Dlod        (ty/make-tfun (ty/make-tcon :sampler) (ty/make-tfun (ty/make-tcon :float4) (ty/make-tcon :float4)))
            'texCUBE         (ty/make-tfun (ty/make-tcon :sampler) (ty/make-tfun (ty/make-tcon :float3) (ty/make-tcon :float4)))
            'clip            (ty/make-tfun (ty/make-tcon :float) nil)
            'discard         (ty/make-tfun nil nil)
            'ddx             (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))
            'ddy             (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))
            'fwidth          (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))
            'lerp            (ty/make-tfun (ty/make-tcon :float) (ty/make-tfun (ty/make-tcon :float) (ty/make-tfun (ty/make-tcon :float) (ty/make-tcon :float))))})))

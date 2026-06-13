(ns top.kzre.homunculus.backend.hlsl.test-utils
  "HLSL 集成测试专用工具。"
  (:require [top.kzre.homunculus.core.types.protocol :as tp]
            [top.kzre.homunculus.core.types.model :as t]))

(defrecord MockHLSLFrontend []
  tp/IFrontendInfo
  (frontend-types [_] [:int :float :bool :float2 :float3 :float4 :float4x4 :texture2D :sampler :vector :map])
  (literal->type [_ val]
    (cond
      (integer? val) (t/->TCon :int)
      (float? val)   (t/->TCon :float)
      (true? val)    (t/->TCon :bool)
      (false? val)   (t/->TCon :bool)
      (keyword? val) (t/->TCon :keyword)
      (nil? val)     (t/->TCon :float)
      :else (t/->TVar (gensym "lit"))))
  (meta->type [_ node]
    (when-let [tag (or (get-in node [:meta :tag])
                       (get-in node [:attrs :tag]))]
      (if (keyword? tag)
        (t/->TCon tag)
        (t/->TCon (keyword (name tag))))))
  (infer-collection-type [_ form] nil)
  (collection-type-ctor [_ kind element-type shape] nil))
(ns top.kzre.homunculus.backend.hlsl.frontend
  "HLSL 前端协议实现：向编译器描述 HLSL 的类型、字面量和内建函数。"
  (:require [top.kzre.homunculus.core.types.protocol :as p]
            [top.kzre.homunculus.core.types.model :as t]))

;; ── HLSL 内置函数类型环境 ─────────────────
(def builtins
  "从 Clojure 符号到 HLSL 函数类型的映射。HM 推断将以此为基础。"
  {'+
   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float)))
   '-
   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float)))
   '*
   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float)))
   '/
   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float)))
   'float4
   (t/->TFun (t/->TCon :float)
             (t/->TFun (t/->TCon :float)
                       (t/->TFun (t/->TCon :float)
                                 (t/->TFun (t/->TCon :float) (t/->TCon :float4)))))
   'float3
   (t/->TFun (t/->TCon :float)
             (t/->TFun (t/->TCon :float)
                       (t/->TFun (t/->TCon :float) (t/->TCon :float3))))
   'float2
   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :float2)))
   'tex2D
   (t/->TFun (t/->TCon :texture2D) (t/->TFun (t/->TCon :sampler) (t/->TCon :float4)))
   'saturate
   (t/->TFun (t/->TCon :float) (t/->TCon :float))
   '<   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :bool)))
   '>   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :bool)))
   '<=  (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :bool)))
   '>=  (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :bool)))
   '=   (t/->TFun (t/->TCon :float) (t/->TFun (t/->TCon :float) (t/->TCon :bool)))
   'not (t/->TFun (t/->TCon :bool) (t/->TCon :bool))
   ;; … 更多内置函数可陆续添加
   })

;; ── HLSL 前端信息实现 ────────────────────
(deftype HLSLFrontend []
  p/IFrontendInfo
  (frontend-types [_]
    [:int :float :bool :float2 :float3 :float4 :float4x4 :texture2D :sampler])

  (literal->type [_ val]
    (cond
      (instance? Long val) (t/->TCon :int)
      (instance? Integer val) (t/->TCon :int)
      (instance? Double val) (t/->TCon :float)
      (instance? Float val) (t/->TCon :float)
      (true? val) (t/->TCon :bool)
      (false? val) (t/->TCon :bool)
      :else (throw (ex-info (str "HLSL unsupported literal: " val) {:val val}))))

  (meta->type [_ node]
    ;; Clojure 元数据可能包含类型标注 ^:float4，或语义标注 ^:SV_Position
    (when-let [tag (get-in node [:meta :tag])]
      (if (keyword? tag)
        ;; 将其视为类型构造器名，如 :float4 → TCon :float4
        (t/->TCon (-> tag name keyword))
        (throw (ex-info "Meta tag must be keyword" {:tag tag})))))

  ;; HLSL 中向量和矩阵被视为内置函数构造，不需要容器推断
  (infer-collection-type [_ form]
    (throw (ex-info "HLSL does not support collection literals" {:form form})))

  (collection-type-ctor [_ kind element-type shape]
    (throw (ex-info "HLSL does not support collection types" {:kind kind})))
  (builtin-functions [_]
    builtins))
(ns top.kzre.homunculus.backend.hlsl.frontend
  "HLSL 前端协议实现：向编译器描述 HLSL 的类型、字面量和内建函数。"
  (:require
    [top.kzre.homunculus.core.types.ho-elim.protocol :as hop]
    [top.kzre.homunculus.core.types.model :as t]
    [top.kzre.homunculus.backend.hlsl.utils :as utils]
    [top.kzre.homunculus.core.types.protocol :as p]))

;; ── HLSL 内置函数类型环境 ─────────────────
(def builtins
  (merge
    ;; 算术二元运算（保持具体，中缀生成以后泛化）
    {'+ (utils/bin-op (t/->TCon :float) (t/->TCon :float))
     '- (utils/bin-op (t/->TCon :float) (t/->TCon :float))
     '* (utils/bin-op (t/->TCon :float) (t/->TCon :float))
     '/ (utils/bin-op (t/->TCon :float) (t/->TCon :float))
     ;; 比较二元运算
     '<  (utils/bin-op (t/->TCon :float) (t/->TCon :bool))
     '>  (utils/bin-op (t/->TCon :float) (t/->TCon :bool))
     '<= (utils/bin-op (t/->TCon :float) (t/->TCon :bool))
     '>= (utils/bin-op (t/->TCon :float) (t/->TCon :bool))
     '== (utils/bin-op (t/->TCon :float) (t/->TCon :bool))
     '!= (utils/bin-op (t/->TCon :float) (t/->TCon :bool))
     ;; 逻辑
     '!  (utils/unary-math (t/->TCon :bool))
     '&& (utils/bin-op (t/->TCon :bool) (t/->TCon :bool))
     '|| (utils/bin-op (t/->TCon :bool) (t/->TCon :bool))
     'not (utils/unary-math (t/->TCon :bool))
     ;; ── 数学函数（泛型一元） ──
     'abs      (utils/generic-unary)
     'saturate (utils/generic-unary)
     'sin      (utils/generic-unary)
     'cos      (utils/generic-unary)
     'tan      (utils/generic-unary)
     'asin     (utils/generic-unary)
     'acos     (utils/generic-unary)
     'atan     (utils/generic-unary)
     'sqrt     (utils/generic-unary)
     'exp      (utils/generic-unary)
     'log      (utils/generic-unary)
     'log2     (utils/generic-unary)
     'log10    (utils/generic-unary)
     'floor    (utils/generic-unary)
     'ceil     (utils/generic-unary)
     'round    (utils/generic-unary)
     'trunc    (utils/generic-unary)
     'frac     (utils/generic-unary)
     'ddx      (utils/generic-unary)
     'ddy      (utils/generic-unary)
     'fwidth   (utils/generic-unary)
     ;; ── 数学函数（泛型二元） ──
     'max   (utils/generic-binary)
     'min   (utils/generic-binary)
     'atan2 (utils/generic-binary)
     'pow   (utils/generic-binary)
     'step  (utils/generic-binary)
     'fmod  (utils/generic-binary)
     ;; ── 三元 ──
     'clamp      (utils/generic-ternary)
     'lerp       (utils/generic-ternary)
     'smoothstep (utils/generic-ternary)
     ;; 向量构造
     'float2 (utils/vector-ctor (t/->TCon :float2) 2)
     'float3 (utils/vector-ctor (t/->TCon :float3) 3)
     'float4 (utils/vector-ctor (t/->TCon :float4) 4)
     ;; 类型转换
     'float (utils/fn-> (t/->TCon :int) (t/->TCon :float))
     'int   (utils/fn-> (t/->TCon :float) (t/->TCon :int))
     'bool  (utils/fn-> (t/->TCon :int) (t/->TCon :bool))
     'int2  (utils/vector-ctor (t/->TCon :int2) 2)
     'int3  (utils/vector-ctor (t/->TCon :int3) 3)
     'int4  (utils/vector-ctor (t/->TCon :int4) 4)
     'bool2 (utils/vector-ctor (t/->TCon :bool2) 2)
     'bool3 (utils/vector-ctor (t/->TCon :bool3) 3)
     'bool4 (utils/vector-ctor (t/->TCon :bool4) 4)
     ;; 向量/矩阵运算
     'dot       (utils/fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float))
     'cross     (utils/fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float3))
     'normalize (utils/generic-unary)
     'length    (utils/fn-> (t/->TCon :float3) (t/->TCon :float))
     'distance  (utils/fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float))
     'reflect   (utils/fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float3))
     'refract   (utils/fn-> (t/->TCon :float3) (t/->TCon :float3) (t/->TCon :float) (t/->TCon :float3))
     'mul       (utils/fn-> (t/->TCon :float4x4) (t/->TCon :float4) (t/->TCon :float4))
     'transpose (utils/unary-math (t/->TCon :float4x4))

     ;; Swizzle
     'sw-x    (utils/fn-> (t/->TCon :float4) (t/->TCon :float))
     'sw-y    (utils/fn-> (t/->TCon :float4) (t/->TCon :float))
     'sw-z    (utils/fn-> (t/->TCon :float4) (t/->TCon :float))
     'sw-w    (utils/fn-> (t/->TCon :float4) (t/->TCon :float))
     'sw-xy   (utils/fn-> (t/->TCon :float4) (t/->TCon :float2))
     'sw-xz   (utils/fn-> (t/->TCon :float4) (t/->TCon :float2))
     'sw-yz   (utils/fn-> (t/->TCon :float4) (t/->TCon :float2))
     'sw-zw   (utils/fn-> (t/->TCon :float4) (t/->TCon :float2))
     'sw-xyz  (utils/fn-> (t/->TCon :float4) (t/->TCon :float3))
     'sw-rgb  (utils/fn-> (t/->TCon :float4) (t/->TCon :float3))
     'sw-xyzw (utils/fn-> (t/->TCon :float4) (t/->TCon :float4))
     'sw-rgba (utils/fn-> (t/->TCon :float4) (t/->TCon :float4))

     ;; 纹理
     'tex2D     (utils/fn-> (t/->TCon :texture2D) (t/->TCon :sampler) (t/->TCon :float4))
     'texCube   (utils/fn-> (t/->TCon :textureCube) (t/->TCon :sampler) (t/->TCon :float4))
     ;; 资源与采样
     'texture2D     (utils/fn-> (t/->TCon :int) (t/->TCon :texture2D))
     'sampler-state (utils/fn-> (t/->TCon :int) (t/->TCon :sampler))
     'sample        (utils/fn-> (t/->TCon :texture2D) (t/->TCon :sampler) (t/->TCon :float4))}))

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
      (nil? val) (do (println "WARNING: nil literal defaulting to float") (t/->TCon :float))
      :else (throw (ex-info (str "HLSL unsupported literal: " val) {:val val}))))

  (meta->type [_ node]
    (when-let [tag (get-in node [:meta :tag])]
      (if (keyword? tag)
        (t/->TCon (-> tag name keyword))
        (throw (ex-info "Meta tag must be keyword" {:tag tag})))))

  (infer-collection-type [_ form]
    (throw (ex-info "HLSL does not support collection literals" {:form form})))

  (collection-type-ctor [_ kind element-type shape]
    (throw (ex-info "HLSL does not support collection types" {:kind kind})))

  (builtin-functions [_]
    builtins))

(defrecord HLSLHoElimConfig []
  hop/IHoElimConfig
  (known-ho-functions [_]
    {'reduce :reduce, 'map :map})
  (supports-dynamic-collections? [_]
    false)
  (backend-length-fn [_]
    'count)
  (backend-nth-fn [_]
    'nth)
  (backend-less-than-fn [_]
    '<))
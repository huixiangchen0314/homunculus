(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。类型以符号形式直接传给宏，展开期构造 IR 类型标记；
   运行时无需类型构造器，类型信息全部通过元数据传给后端。"
  (:require
    [top.kzre.homunculus.backend.hlsl.lang :as hlsl.lang]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;;; ========= IR 类型辅助（宏展开期使用）=========

(defn- ir-type
  [type-sym]
  (ty/make-tcon (symbol (name type-sym))))

(defn- ir-array-type
  [elem-sym size]
  (ty/make-tvec (ir-type elem-sym) (ty/make-tvalue size)))

;;; ========= 入口 =========

(defmacro defshader
  [stage name param-vec & body]
  (let [meta-map (merge {:shader/stage stage :shader/entry? true}
                        (meta name)
                        (meta param-vec))
        fn-form (list* 'fn param-vec body)]
    `(def ~(vary-meta name merge meta-map) ~fn-form)))

;;; ========= 资源与变量（声明，无值）=========

(defmacro defuniform
  "定义全局 uniform（声明，无值）。
   示例：(defuniform worldViewProj float4x4)
         (defuniform lights float4 8)"
  ([var-name elem-sym]
   (let [ir-ty (ir-type elem-sym)]
     `(def ~(vary-meta var-name assoc
                       :tag ir-ty
                       :shader/uniform? true
                       ))))
  ([var-name elem-sym size]
   (let [ir-ty (ir-array-type elem-sym size)]
     `(def ~(vary-meta var-name assoc
                       :shader/uniform? true
                       :tag ir-ty)))))

(defmacro defstatic
  "定义全局静态变量。

   - (defstatic name type-sym)           无值，后端按类型默认初始化
   - (defstatic name init-expr)          有值，类型由 init-expr 的构造器推断
   - (defstatic name type-sym init-expr) 有值，显式类型（init 可为任意表达式）"
  ([name type-or-ctor]
   (if (symbol? type-or-ctor)
     ;; 无值：type-or-ctor 是类型
     (let [ir-ty (ir-type type-or-ctor)]
       `(def ~(vary-meta name assoc
                         :tag ir-ty
                         :shader/static-var? true)))
     ;; 有值：type-or-ctor 是表达式。类型取构造器首元素。
     (let [ctor  (when (and (seq? type-or-ctor) (symbol? (first type-or-ctor)))
                   (first type-or-ctor))
           ir-ty (when ctor (ir-type ctor))]
       `(def ~(vary-meta name assoc
                         :tag ir-ty
                         :shader/static-var? true)
          ~type-or-ctor))))
  ([name type-sym init-expr]
   (let [ir-ty (ir-type type-sym)]
     `(def ~(vary-meta name assoc
                       :tag ir-ty
                       :shader/static-var? true)
        ~init-expr))))

(defmacro deftexture
  "定义纹理资源（声明，无值）。
   示例：(deftexture albedo t0)
         (deftexture albedo t0 :2d float4)"
  ([name register]
   `(deftexture ~name ~register :2d float4))
  ([name register dimension]
   `(deftexture ~name ~register ~dimension float4))
  ([sym register dimension elem-sym]
   (let [texture-type-sym (hlsl.lang/texture-dimension->type-sym dimension)
         element-type     (name elem-sym)
         ir-ty            (ir-type texture-type-sym)]
     `(def ~(vary-meta sym assoc
                       :tag ir-ty
                       :shader/resource? true
                       :shader/resource-kind :texture
                       :shader/texture-register register
                       :shader/dimension dimension
                       :shader/element-type element-type)))))

(defmacro defsampler
  "定义采样器资源（声明，无值）。"
  ([name register]
   `(defsampler ~name ~register false))
  ([sym register-kw compare]
   (let [sampler-type-sym (if compare 'SamplerComparisonState 'SamplerState)
         ir-ty            (ir-type sampler-type-sym)]
     `(def ~(vary-meta sym assoc
                       :tag ir-ty
                       :shader/resource? true
                       :shader/resource-kind :sampler
                       :shader/sampler-compare? (boolean compare)
                       :shader/sampler-register register-kw)))))

(defmacro defcbuffer
  "定义 cbuffer 资源。成员以交替的 符号+类型符号 给出，同时为每个成员生成隐式声明 def。"
  [name register-kw & members]
  (let [pairs      (partition 2 members)
        member-map (into {} (map (fn [[sym type-sym]] [sym type-sym]) pairs))
        member-defs
        (map (fn [[sym type-sym]]
               (let [ir-ty (ir-type type-sym)]
                 `(def ~(vary-meta sym assoc
                                   :shader/cbuffer-member true
                                   :shader/ignore-emit? true
                                   :tag ir-ty))))
             pairs)]
    `(do
       (def ~(vary-meta name assoc
                        :shader/resource? true
                        :shader/resource-kind :cbuffer
                        :shader/cbuffer-register register-kw
                        :shader/cbuffer-members member-map))
       ~@member-defs)))
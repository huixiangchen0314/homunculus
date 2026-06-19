;; top.kzre.homunculus.backend.shader.dsl
(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。提供统一的资源/入口声明，通过元数据传递给后端。"
  (:refer-clojure :exclude [float]))

(defmacro defshader
  "定义着色器入口函数。stage 为 :vertex, :fragment 等，params 为带类型标注的参数向量。"
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn params body)
                 {:shader-stage stage
                  :shader/entry? true})))

;; 类型构造器（运行时无操作，仅用于类型标记）
(defn- texture2D     [] nil)
(defn- sampler-state [] nil)
(defn- cbuffer       [] nil)
;; 声明专用类型构造器.
(defn float          [] nil)
(defn float2         [] nil)
(defn float3         [] nil)
(defn float4         [] nil)
(defn float4x4       [] nil)
;; 根据短名返回全限定类型构造器符号
(defn- fully-qualified-ctor [ctor-sym]
  (let [ctor-name (name ctor-sym)]
    (cond
      (= "float"    ctor-name) `top.kzre.homunculus.backend.shader.dsl/float
      (= "float2"   ctor-name) `top.kzre.homunculus.backend.shader.dsl/float2
      (= "float3"   ctor-name) `top.kzre.homunculus.backend.shader.dsl/float3
      (= "float4"   ctor-name) `top.kzre.homunculus.backend.shader.dsl/float4
      (= "float4x4" ctor-name) `top.kzre.homunculus.backend.shader.dsl/float4x4
      (= "texture2D"     ctor-name) `top.kzre.homunculus.backend.shader.dsl/texture2D
      (= "sampler-state" ctor-name) `top.kzre.homunculus.backend.shader.dsl/sampler-state
      (= "cbuffer"       ctor-name) `top.kzre.homunculus.backend.shader.dsl/cbuffer
      :else (throw (ex-info (str "Unknown type constructor: " ctor-name) {:ctor ctor-name})))))

(defmacro defuniform
  "定义全局 uniform 常量。示例：(defuniform worldViewProj float4x4)"
  [name type-ctor]
  `(def ~(vary-meta name assoc :shader/uniform? true)
     (~(fully-qualified-ctor type-ctor))))

(defmacro defstatic
  "定义全局静态变量专用宏. eg. (defstatic accumColor (float4 0.0 0.0 0.0 0.0))"
  [name type-ctor]
  `(def ~(vary-meta name assoc :shader/static-var? true) ~type-ctor))    ;; 不展开成声明专用类型构造器

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
     (sampler-state)))

(defmacro defcbuffer
  "定义 cbuffer 资源。成员以交替的符号+类型构造器给出，如 lightDir float3。
   同时为每个成员生成隐式 def，使类型系统可见。"
  [name register-kw & members]
  (let [pairs (partition 2 members)
        map-expr (into {} (map (fn [[sym type-ctor]]
                                 [(keyword sym) (fully-qualified-ctor type-ctor)])
                               pairs))
        member-defs (map (fn [[sym type-ctor]]
                           (let [full-ctor (fully-qualified-ctor type-ctor)]
                             `(def ~(vary-meta sym assoc
                                               :shader/cbuffer-member true
                                               :shader/ignore-emit? true)
                                (~full-ctor))))
                         pairs)]
    `(do
       ~@member-defs
       (def ~(vary-meta name assoc
                        :shader/resource? true
                        :shader/resource-kind :cbuffer
                        :shader/cbuffer-register register-kw
                        :shader/cbuffer-members map-expr)
         (top.kzre.homunculus.backend.shader.dsl/cbuffer)))))
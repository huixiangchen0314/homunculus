;; top.kzre.homunculus.backend.shader.dsl
(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。提供统一的资源/入口声明，通过元数据传递给后端。")

(defmacro defshader
  "定义着色器入口函数。stage 为 :vertex, :fragment 等，params 为带类型标注的参数向量。"
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn* params body)
                 {:shader-stage stage
                  :shader/entry? true})))

(defmacro defuniform
  "定义全局 uniform 常量。"
  [name val]
  `(def ~(vary-meta name assoc :shader/uniform? true) ~val))

;; 资源构造器（运行时无操作，仅用于元数据标记）
(defn texture2D  [] nil)
(defn sampler-state [] nil)
(defn cbuffer [] nil)

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
  "定义 cbuffer 资源。寄存器与成员信息通过元数据传递。"
  [name register-kw & members]
  (let [pairs (map (fn [[sym type]] [(keyword sym) type]) members)
        map-expr (into {} pairs)]
    `(def ~(vary-meta name assoc
                      :shader/resource? true
                      :shader/resource-kind :cbuffer
                      :shader/cbuffer-register register-kw
                      :shader/cbuffer-members map-expr)
       (cbuffer))))
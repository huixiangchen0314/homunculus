(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。")

(defmacro defshader
  "定义着色器入口函数。"
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn* (vec params) body) {:shader-stage stage})))


;; 资源构造器：普通 Clojure 函数，类型由 typed 推断（前端 builtins 提供）
(defn texture2D     [register] nil)
(defn sampler-state [register] nil)
(defn cbuffer       [register & members] nil)


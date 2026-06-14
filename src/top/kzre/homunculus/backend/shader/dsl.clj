(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。")

(defmacro defshader
  "定义着色器入口函数。"
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn* (vec params) body) {:shader-stage stage})))



(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。")

(defmacro defshader
  "定义着色器入口函数。stage 为 :vertex, :fragment 等，params 为带类型标注的参数向量。"
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn* params body) {:shader-stage stage})))

(defmacro defuniform
  "定义全局 uniform 常量。展开为带 :uniform 元数据的 def。"
  [name val]
  `(def ~(vary-meta name assoc :uniform true) ~val))

;; 资源构造器：普通 Clojure 函数，类型由 typed 推断（前端 builtins 提供）
(defn texture2D     [register] nil)
(defn sampler-state [register] nil)
(defn cbuffer [register members] nil)

(defmacro defcbuffer [name register-kw register-idx & members]
  ;; members 的每个元素都是 [symbol expr] 对，直接处理
  (let [pairs (map (fn [[k v]] [(keyword k) v]) members)
        map-expr (into {} pairs)]
    `(def ~name (cbuffer ~register-idx ~map-expr))))
(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖，提供 defshader 等宏。")


(defn- parse-param
  "解析单个参数：[^:semantic name type-keyword] → 符号（带元数据）"
  [[meta-sym name type]]
  (let [semantic (-> meta-sym name keyword)]   ;; 将 ^:SV_Position 转成 :SV_Position
    (vary-meta name assoc semantic true)))

(defmacro defshader
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn* (vec params) body) {:shader-stage stage})))



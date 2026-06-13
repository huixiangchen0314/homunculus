(ns top.kzre.homunculus.backend.shader.dsl
  "着色器 DSL 语法糖。")

(defmacro defshader
  "定义着色器入口函数。"
  [stage name params & body]
  `(def ~name
     ~(with-meta (list* 'fn* (vec params) body) {:shader-stage stage})))


(defmacro texture2D [& {:keys [register]}]
  {:resource-type :texture2D :register register})

(defmacro sampler [& {:keys [register]}]
  {:resource-type :sampler :register register})

(defmacro cbuffer [members & {:keys [register]}]
  (let [member-specs (mapv (fn [m]
                             (let [sym (if (symbol? m) m (first m))
                                   meta (meta sym)]
                               {:name (name sym)
                                :type (some (fn [k] (when (keyword? k) k)) (keys meta))}))
                           members)]
    {:resource-type :cbuffer :register register :members member-specs}))
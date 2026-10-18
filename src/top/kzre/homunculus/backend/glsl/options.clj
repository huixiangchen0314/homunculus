(ns top.kzre.homunculus.backend.glsl.options
  "GLSL 后端配置项的统一读取入口。。")

(defn gl-version
  [options]
  (get options :gl-version 330))

(defn out-struct-strategy
  [options]
  (get options :gl-out-struct-strategy :split))

(defn ns-strategy
  "命名空间处理策略。

   :strip  —— 去掉命名空间，只保留 name 部分。
              如 top.kzre.../myTexture → myTexture
   :mangle —— 全限定名整体转义，ns 的 . 和 / 都替换为 _
              如 top.kzre.../myTexture → top_kzre_..._myTexture

   options 键：:gl-ns-strategy，默认 :strip。"
  [options]
  (get options :gl-ns-strategy :strip))

(defn fragment-out-name
  [options]
  (get options :gl-fragment-out-name "fragColor"))

(defn texture-sampler-binding-strategy
  [options]
  (get options :gl-texture-sampler-binding-strategy :sampler-first))

(defn vertex-semantic-location
  [options semantic]
  (get-in options [:gl-vertex-semantic-locations semantic]))

(defn fragment-semantic-location
  [options semantic]
  (get-in options [:gl-fragment-semantic-locations semantic]))

(defn texture-sampler-binding
  [options]
  (:gl-texture-sampler-binding options))

(defn interface-block-naming
  [options]
  (:gl-interface-block-naming options))

(defn interface-block-instance-naming
  [options]
  (:gl-interface-block-instance-naming options))

(defn sampler-naming-strategy
  "采样器命名策略。特别的，采样器不存在时候，只使用纹理命名
  :texture-sampler 纹理_采样器
  :sampler-texture
  :texture-sampler-simple 纹理_采样器，特别的，当不冲突时候，只使用 纹理命名，
  :sampler-texture-simple"
  [options]
  (:gl-sampler-naming-strategy options :texture-sampler-simple))
(ns top.kzre.homunculus.backend.glsl.config
  (:require
    [top.kzre.homunculus.backend.glsl.semantic.location :as location]
    [top.kzre.homunculus.internal.protocol :as proto]
    [top.kzre.homunculus.core.error :as err]))

(defn gl-version
  "GLSL 版本号。写入 `#version <N> core`。

   键：:gl-version，默认 330（GLSL 3.30 core）。"
  [config]
  (or (proto/get-option config :gl-version)  330))


(defn ns-strategy
  "命名空间处理策略。在 ShaderAST 层对所有符号统一应用。

   :strip  —— 去掉命名空间，只保留 name 部分。
              如 top.kzre.../myTexture → myTexture
   :mangle —— 全限定名整体转义，ns 的 . 和 / 都替换为 _
              如 top.kzre.../myTexture → top_kzre_..._myTexture

   键：:gl-ns-strategy，默认 :strip。"
  [config]
  (or (proto/get-option config :gl-ns-strategy) :strip))

(defn out-struct-strategy
  "顶点着色器返回 struct 时的输出展开策略。

   :split           —— 每个字段生成独立 out 变量。VS out 和 VS in
                       在同一全局作用域，同名字段加 _<inc> 后缀避让。
   :interface-block —— 生成命名接口块 `out BlockName { ... } instance;`。
                       块 instance 隔离 in/out 作用域，不需要改名。
                       fragment 侧生成同名 in block——按块名 + 成员名
                       匹配，不依赖 location。

   键：:gl-out-struct-strategy，默认 :split。"
  [config]
  (or (proto/get-option config :gl-out-struct-strategy) :split))

(defn semantic-location-strategy
  "semantic 到 location 的分配策略。三个 location 空间各自独立调用。

   :auto           —— 按声明顺序从 0 分配，忽略配置表
   :mapping        —— 优先查表；未命中的从已用最大值 +1 顺序补位
   :mapping-strict —— 必须查表命中，否则抛 :gl-no-location-for-semantic

   键：:gl-semantic-location-strategy，默认 :mapping。"
  [config]
  (or (proto/get-option config :gl-semantic-location-strategy) :mapping))

(defn vertex-semantic-location
  "vertex-input 空间的 location。按 semantic 查用户配置，未命中
   回退到默认表（:position 0 / :normal 1 / :texcoord0 2 / ...）。

   配置键：:gl-vertex-semantic-locations（map，semantic → location）
   返回 nil 表示该 semantic 在 vertex-input 空间没有默认值——
   :mapping 策略下由 assign-locations 补位，:mapping-strict 抛错。"
  [config semantic]
  (or (proto/get-option config [:gl-vertex-semantic-locations semantic])
      (location/default-vertex-input-location semantic)))

(defn vertex-semantics
  "vertex-input 空间所有已定义语义的集合。
   默认表 key ∪ 用户配置 :gl-vertex-semantic-locations 的 key。"
  [config]
  (into (set (keys (location/default-vertex-input-locations)))
        (keys (proto/get-option config :gl-vertex-semantic-locations))))

(defn varying-location-enabled?
  [config]
  (let [enabled? (or (proto/get-option config :gl-varying-location-enabled?) false)]
    (when enabled?
      (let [ver (gl-version config)]
        (when (< ver 410)
          (throw (err/compile-error
                   :gl-unsupported-capability
                   "varying-location-enabled? is not supported in current version, at least 410."
                   {:gl-version ver})))))
    enabled?))

(defn varying-semantic-location
  "varying 空间的 location。按 semantic 查用户配置，未命中回退到
   默认表（:normal 0 / :texcoord0 1 / ...）。

   varying 空间由 vertex out 和 fragment in 共享——两侧查同一张表，
   同 semantic 拿到同 location，链接对齐。

   配置键：:gl-varying-semantic-locations（map，semantic → location）
   返回 nil 表示该 semantic 在 varying 空间没有默认值。"
  [config semantic]
  (or (proto/get-option config [:gl-varying-semantic-locations semantic])
      (location/default-varying-location semantic)))

(defn varying-semantics
  "varying 空间所有已定义语义的集合。
   默认表 key ∪ 用户配置 :gl-varying-semantic-locations 的 key。"
  [config]
  (into (set (keys (location/default-varying-locations)))
        (keys (proto/get-option config :gl-varying-semantic-locations))))

(defn fragment-semantic-location
  "fragment-output 空间的 location。按 semantic 查用户配置，未命中
   回退到默认表（:target0 0 / :target1 1 / ...）。

   单 fragColor 不走这里——plan-fragment-return 的标量分支硬编码
   location 0。多 render target 的 struct 返回字段查这张表。

   配置键：:gl-fragment-semantic-locations（map，semantic → location）
   返回 nil 表示该 semantic 在 fragment-output 空间没有默认值。"
  [config semantic]
  (or (proto/get-option config [:gl-fragment-semantic-locations semantic])
      (location/default-fragment-output-location semantic)))

(defn fragment-semantics
  "fragment-output 空间所有已定义语义的集合。
   默认表 key ∪ 用户配置 :gl-fragment-semantic-locations 的 key。"
  [config]
  (into (set (keys (location/default-fragment-output-locations)))
        (keys (proto/get-option config :gl-fragment-semantic-locations))))

(defn texture-sampler-binding
  "逐对指定 texture + sampler 组合的 texture unit。仅在
   :texture-sampler-binding-strategy 为 :mapping 时使用。

   三种形态：
   ▸ map  —— {[tex-reg sampler-reg] unit}，查不到时抛错
   ▸ fn   —— (fn [tex-reg sampler-reg] unit)
   ▸ nil  —— :mapping 策略下未配置，抛错

   键：:gl-texture-sampler-binding，无默认值（nil）。"
  [config]
  (proto/get-option config :gl-texture-sampler-binding))

(defn interface-block-naming
  "接口块的块名生成。在 :interface-block 策略下，vertex struct 和
   fragment struct 映射到同名 GLSL block——GLSL 链接要求块名一致。

   三种形态：
   ▸ fn   —— (fn [struct-name] block-name)
   ▸ map  —— {struct-name block-name}，查不到时抛错
   ▸ nil  —— 默认用 struct 的 name 部分

   键：:gl-interface-block-naming，无默认值（nil 走 name 兜底）。"
  [config]
  (proto/get-option config :gl-interface-block-naming ))

(defn interface-block-instance-naming
  "接口块的实例名生成。同名 block 在 vertex 和 fragment 两侧用不同
   instance 名——块名是链接依据，实例名是各自作用域内的变量。

   三种形态：
   ▸ fn   —— (fn [struct-name stage direction] instance-name)
   ▸ nil  —— 默认用 _<struct>_<direction>_<hash> 形式

   stage 是 :vertex / :fragment，direction 是 :in / :out。

   键：:gl-interface-block-instance-naming，无默认值（nil 走内置规则）。"
  [config]
  (proto/get-option config :gl-interface-block-instance-naming))

(defn sampler-naming-strategy
  "sampler uniform 的 GLSL 名生成策略。

   :texture-sampler        —— 纹理_采样器（tex_smp）
   :sampler-texture        —— 采样器_纹理（smp_tex）
   :texture-sampler-simple —— 不冲突时只用纹理名，同 texture 配多个
                              sampler 时用 纹理_采样器
   :sampler-texture-simple —— 不冲突时只用采样器名，同 sampler 配多个
                              texture 时用 采样器_纹理

   sampler 不存在（load-only）时一律只用纹理名——不追加后缀。

   键：:gl-sampler-naming-strategy，默认 :texture-sampler-simple。"
  [config]
  (or (proto/get-option config :gl-sampler-naming-strategy)  :texture-sampler-simple))

(defn fragment-out-name
  "fragment 单值返回时的输出变量名。

   标量返回（非 struct）的 fragment entry 生成
   `layout(location = 0) out vec4 <name>;`。

   键：:gl-fragment-out-name，默认 \"fragColor\"。"
  [config]
  (or (proto/get-option config :gl-fragment-out-name) "fragColor"))

(defn texture-sampler-binding-strategy
  "texture + sampler 组合到 GLSL sampler uniform 的 unit 分配策略。

   :sampler-first —— 按 sampler 寄存器分组排序，组内按 texture 寄存器
   :texture-first —— 按 texture 寄存器分组排序，组内按 sampler 寄存器
   :mapping       —— 用户通过 :gl-texture-sampler-binding 逐对指定 unit

   键：:gl-texture-sampler-binding-strategy，默认 :sampler-first。"
  [config]
  (or (proto/get-option config :gl-texture-sampler-binding-strategy) :sampler-first))
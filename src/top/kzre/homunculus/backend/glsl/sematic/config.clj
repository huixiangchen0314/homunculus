(ns top.kzre.homunculus.backend.glsl.sematic.config
  (:require
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IConfig
  (gl-version [_]
    "目标 GL 版本。目前只支持 330。
     options 键：:gl-version")

  (out-struct-strategy [_]
    "struct 类型作为入口点返回时的展开策略。

     :split           —— 每个字段展开为独立的 out / varying 变量
     :interface-block —— 生成 GLSL 接口块（受 stage 限制）

     options 键：:gl-out-struct-strategy，默认 :split。")

  (fragment-out-name [_]
    "fragment 单值返回时的输出变量名。

     options 键：:gl-fragment-out-name，默认 fragColor。")

  (vertex-semantic-location [_ semantic]
    "顶点输入的语义 → GLSL location 映射。

     带数字的语义可以从名字解析——:texcoord0 → 0、:texcoord1 → 1、
     :color0 → 0（与 TEXCOORD 共享槽位空间，需要时区分）。
     不带数字的语义（:position / :normal / :tangent）从 options 读：
       :gl-vertex-semantic-locations {:position 0 :normal 1 :tangent 2}

     未配置且无法从语义名解析时抛异常。")

  (fragment-semantic-location [_ semantic]
    "片元输出的语义 → GLSL location 映射。

     :target0 → 0、:target1 → 1 可以从名字解析。
     :depth 映射为 gl_FragDepth（内置，不占 location）。
     options 可覆盖：:gl-fragment-semantic-locations")
  (texture-sampler-binding-strategy [_]
    "纹理-采样器合并后的 binding（texture unit）分配策略。

     :sampler-first —— 按 sampler 的寄存器号分组排序，
                       组内按 texture 的寄存器号排序
     :texture-first —— 按 texture 的寄存器号分组排序，
                       组内按 sampler 的寄存器号排序
     :mapping       —— 逐对调用 map-texture-sampler-binding 获取 binding

     options 键：:gl-texture-sampler-binding-strategy
     默认 :sampler-first")

  (map-texture-sampler-binding [_ tex-reg sampler-reg]
    "mapping 策略下使用。接收纹理寄存器和采样器寄存器，
     返回该组合的 texture unit 序号。

     tex-reg / sampler-reg 是关键字（:t0 / :s0）或 nil。
     options 键：:gl-texture-sampler-binding（函数/map）
     mapping 策略下未配置此函数时抛异常。")
  (interface-block-naming [_ struct-name]
    "从 struct 名派生接口块的命名。

     返回 {:block-name ... :instance-name ...}：
       block-name    —— 链接匹配用，VS 和 FS 必须一致。
                        默认 (name struct-name)。
       instance-name —— 实例名，访问成员用 instance.member。
                        不参与链接。默认 (str 'v_' block-name)。

   options 可覆盖：
     :gl-interface-block-name-fn    —— (fn [struct-name] → block-name)
     :gl-interface-block-instance-fn —— (fn [struct-name] → instance-name)")
  (interface-block-instance-naming [_ struct-name stage direction]
    "从 struct 名、stage、方向派生接口块的实例名。

     实例名是 shader 内的局部变量名——用于访问成员
     instance.member。不参与链接匹配，VS 和 FS 可以不同。

     direction —— :in / :out
     stage     —— :vertex / :fragment

     默认规则：\"_\" + direction + \"_\" + struct-name。
       例：VS 的 out 块 → \"_out_VertexData\"
           FS 的 in 块  → \"_in_VertexData\"

     options 可覆盖：:gl-interface-block-instance-naming
       可以是：
         - fn  —— (fn [struct-name stage direction] → instance-name)
         - nil —— 默认规则"))

(defrecord Config [compile-ctx options]
  IConfig
  (gl-version [_] (get options :gl-version 330))
  (out-struct-strategy [_] (get options :gl-out-struct-strategy :split))
  (fragment-out-name [_] (get options :gl-fragment-out-name "fragColor"))
  (vertex-semantic-location [_ semantic]
    (get-in options [:gl-vertex-semantic-locations semantic]))
  (fragment-semantic-location [_ semantic]
    (get-in options [:gl-fragment-semantic-locations semantic]))
  (texture-sampler-binding-strategy [_]
    (get options :gl-texture-sampler-binding-strategy :sampler-first))
  (map-texture-sampler-binding [_ tex-reg sampler-reg]
    (let [v (:gl-texture-sampler-binding options)]
      (cond
        (fn? v)
        (v tex-reg sampler-reg)

        (map? v)
        (let [unit (get v [tex-reg sampler-reg])]
          (if (nil? unit)
            (throw (ex-info "no binding mapping for combination"
                            {:tex-reg tex-reg :sampler-reg sampler-reg
                             :map-keys (keys v)}))
            unit))

        (nil? v)
        (throw (ex-info "mapping strategy requires :gl-texture-sampler-binding"
                        {:tex-reg tex-reg :sampler-reg sampler-reg}))

        :else
        (throw (ex-info "unsupported :gl-texture-sampler-binding type"
                        {:type (type v)
                         :value v})))))

  (interface-block-naming [_ struct-name]
    (let [v (:gl-interface-block-naming options)]
      (cond
        (fn? v)
        (v struct-name)

        (map? v)
        (or (get v struct-name)
            (throw (ex-info "no interface-block name for struct"
                            {:struct-name struct-name
                             :map-keys    (keys v)})))

        (nil? v)
        (name struct-name)

        :else
        (throw (ex-info "unsupported :gl-interface-block-naming type"
                        {:type  (type v)
                         :value v})))))
  (interface-block-instance-naming [_ struct-name stage direction]
    (let [v (:gl-interface-block-instance-naming options)]
      (cond
        (fn? v)
        (v struct-name stage direction)

        (nil? v)
        (str "_"
             (name struct-name) "_"
             (name direction) "_"
             (Integer/toHexString
               (hash [struct-name stage direction])))

        :else
        (throw (ex-info "unsupported :gl-interface-block-instance-naming type"
                        {:type (type v) :value v}))))))

(defn make-config [compile-ctx]
  (let [options (ip/options compile-ctx)]
    (->Config compile-ctx options)))
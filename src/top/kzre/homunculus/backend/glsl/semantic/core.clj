(ns top.kzre.homunculus.backend.glsl.semantic.core
  "语义降级规划的 lower 实现。

   ── 职责 ──────────────────────────────────────────────
   根据 SLAnalysis + 当前 stage + 该入口点组的 asts，生成 GLSL
   降级所需的规划数据：
   ▸ sampler 规划——每个 (texture, sampler) 组合 → SamplerUniform，
     按 load-only 规则区分是否带 sampler
   ▸ 入口点规划——入口点的 params / return → in/out IODecl 或
     InterfaceBlock，按 out-struct-strategy 展开
   ▸ location 分配——按 semantic-location-strategy 为每个非 builtin
     的 in/out 分配 layout(location = N)
   ▸ varying 命名——vertex out 和 fragment in 的变量名必须一致。
     不冲突时用原名；和 entry 输入参数名冲突时加 _<inc> 后缀。
     vertex 侧计算，fragment 侧查同一映射。

   ── location 三空间 ──────────────────────────────────
   GLSL 有三个独立的 location 空间：
   ▸ vertex-input    —— 顶点属性槽位，plan-vertex-inputs 用
   ▸ varying         —— vertex out / fragment in 共享，plan-vertex-return
                        （:split）和 plan-fragment-inputs 用
   ▸ fragment-output —— render target 槽位，plan-fragment-return 用

   同 semantic 在不同空间占不同槽位——:normal 在 vertex-input 是 1，
   在 varying 是 0。

   ── varying-location-enabled? ────────────────────────
   GLSL 330 的 varying 不支持 layout(location)——需要 GLSL 410
   或 ARB_separate_shader_objects。配置的 varying-location-enabled?
   为 false 时：
   ▸ varying 空间的 IODecl 的 :location 是 nil
   ▸ emit 层不输出 layout(location)——链接按名称匹配
   ▸ vertex-input / fragment-output 空间不受影响——330 支持

   ── 跨阶段联动 ────────────────────────────────────────
   vertex 用 :interface-block 策略返回 struct 时，fragment 输入必须
   用同名 block 接收（GLSL 链接规则）。即使 fragment 声明为离散参数，
   也生成对应的 block——非 builtin 参数不再生成独立 IODecl，
   wrapper 在 emit 阶段从 block instance 按 semantic 读成员。

   ── location 分配策略 ────────────────────────────────
   :auto           —— 按声明顺序从 0 分配，忽略配置表
   :mapping        —— 优先查表；查不到的从已用最大值 +1 顺序补位
   :mapping-strict —— 必须查表命中，否则抛错

   ── 语言层工具 ────────────────────────────────────────
   类型映射、sampler 类型推导、flat 判定、内置语义走 glsl.lang；
   struct 类型判定走 hlsl.lang。本层只负责规划编排。

   ── 不在本层做的事 ────────────────────────────────────
   ▸ uniform block 的 binding——逐节点翻译，尊重 register(bN)
   ▸ vertex input struct 参数——当前只处理离散参数
   ▸ 名字冲突检测——假设 HLSL 变量名与 GLSL 生成名不冲突"
  (:require
    [top.kzre.homunculus.backend.glsl.lang :as lang]
    [top.kzre.homunculus.backend.glsl.semantic.config :as config]
    [top.kzre.homunculus.backend.glsl.semantic.layout :as layout
     :refer [->Layout ->SamplerUniform]]
    [top.kzre.homunculus.backend.hlsl.lang :as hlsl.lang]
    [top.kzre.homunculus.frontend.shader.ast :as ast]
    [top.kzre.homunculus.frontend.shader.pass.analyze.model :as analysis]
    [top.kzre.homunculus.frontend.shader.semantic :as semantic]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.error :as err])
  (:import
    (top.kzre.homunculus.backend.glsl.semantic.layout
      Layout)
    (top.kzre.homunculus.frontend.shader.pass.analyze.model
      SLAnalysis SLComputeEntry SLGraphicsEntry SLParam SLReturn
      SLSampleCall SLSampler SLTexture)))

;; ═══════════════════════════════════════════
;; AST 辅助
;; ═══════════════════════════════════════════

(defn- collect-struct-names
  [asts]
  (into #{}
        (keep #(when (= :struct (ast/kind %)) (:name %)))
        asts))

(defn- find-struct
  [asts struct-name]
  (or (some (fn [n]
              (when (and (= :struct (ast/kind n))
                         (= struct-name (:name n)))
                n))
            asts)
      (throw (err/compile-error
               :gl-struct-not-found
               "struct definition not found"
               {:struct struct-name}))))

(defn- vertex-return-type
  [^SLAnalysis analysis]
  (some-> (analysis/find-entry analysis :vertex) :return :type))

(defn- vertex-return-struct
  "vertex 返回 struct 类型时，返回其 AST 定义；否则 nil。"
  [^SLAnalysis analysis struct-names asts]
  (let [v-ty (vertex-return-type analysis)]
    (when (and v-ty (hlsl.lang/struct-type? v-ty struct-names))
      (find-struct asts (ty/type-sym v-ty)))))

;; ═══════════════════════════════════════════
;; location 分配
;; ═══════════════════════════════════════════

(defn- allocate-semantic-locations
  "为 semantic 列表分配 location。

   :auto           —— 按声明顺序从 0 分配，忽略配置表
   :mapping        —— 优先查表；查不到的从已用最大值 +1 顺序补位
   :mapping-strict —— 必须查表命中，否则抛错

   返回 semantic → location 的 map。
   semantic 重复或分配结果 location 重复抛错。

   注意：每个 location 空间独立调用一次——vertex-input、varying、
   fragment-output 各自分配。跨空间不对齐是预期的——它们本来就是
   不同的槽位。"
  [semantics cfg lookup-fn]
  (when (not= (count semantics) (count (distinct semantics)))
    (throw (err/compile-error
             :gl-duplicate-semantic
             "duplicate semantic in location assignment"
             {:semantics semantics})))
  (let [strategy   (config/semantic-location-strategy cfg)
        check-locs (fn [m]
                     (when (not= (count m) (count (set (vals m))))
                       (throw (err/compile-error
                                :gl-duplicate-location
                                "duplicate location assignment"
                                {:semantic-locations m})))
                     m)]
    (case strategy

      :auto
      (check-locs (into {} (map-indexed (fn [i sem] [sem i]) semantics)))

      :mapping
      (let [explicit  (into {} (keep (fn [sem]
                                       (when-let [loc (lookup-fn sem)]
                                         [sem loc])))
                            semantics)
            init-next (inc (reduce max -1 (vals explicit)))
            merged    (first
                        (reduce (fn [[m next] sem]
                                  (if (contains? m sem)
                                    [m next]
                                    [(assoc m sem next) (inc next)]))
                                [explicit init-next]
                                semantics))]
        (check-locs merged))

      :mapping-strict
      (check-locs
        (into {} (map (fn [sem]
                        (let [loc (lookup-fn sem)]
                          (when (nil? loc)
                            (throw (err/compile-error
                                     :gl-no-location-for-semantic
                                     "no location configured for semantic"
                                     {:semantic sem})))
                          [sem loc])))
              semantics))
      (throw (err/compile-error
               :gl-unknown-location-strategy
               "unknown semantic-location-strategy"
               {:strategy strategy})))))

(defn- binding-semantics-of-struct
  "从 struct 定义中提取非 builtin 字段的 semantic 列表。"
  [stage struct-decl]
  (into []
        (keep (fn [f]
                (let [sem (semantic/shader-semantic f)]
                  (when-not (lang/builtin-semantic? stage sem)
                    sem))))
        (:members struct-decl)))

;; ═══════════════════════════════════════════
;; varying 命名
;; ═══════════════════════════════════════════

(defn- normalize-name
  "计算 varying 变量名。不冲突用原名；和 excluded-names 冲突时
   加 _1、_2 递增后缀。

   vertex out 和 fragment in 用同一规则、同一 exclude 集合（VS
   entry 的 params 名），得到一致的名字——链接按 location 匹配，
   但名字一致让源码可读性更好，也避免用户混淆。
   此外，低版本的 gl 不支持 binding 映射，名称必须相同。"
  [base-name excluded-names]
  (if-not (contains? excluded-names base-name)
    base-name
    (loop [i 1]
      (let [candidate (symbol (str (name base-name) "_" i))]
        (if (contains? excluded-names candidate)
          (recur (inc i))
          candidate)))))

(defn- vertex-varying-name-mapping
  "纯计算：给定 struct 定义和排除名集合，生成 semantic → varying 名。
   只处理非 builtin 字段。"
  [struct-decl excluded-names]
  (into {}
        (keep (fn [f]
                (let [sem (semantic/shader-semantic f)]
                  (when-not (lang/builtin-semantic? :vertex sem)
                    [sem (normalize-name (:name f) excluded-names)]))))
        (:members struct-decl)))

(defn- vertex-varying-name-mapping-by-analysis
  "计算 VS out 的 varying 变量名映射：semantic → var-name。

   exclude 集合是 VS entry 的 params 名——VS out 和 VS in 在同一
   全局作用域，同名非法。fragment 侧查这个映射拿和 VS 一致的 in
   变量名。

   VS 不存在或不是 struct 返回时返回空 map。"
  [^SLAnalysis analysis struct-names asts]
  (if-let [v-struct (vertex-return-struct analysis struct-names asts)]
    (let [v-entry (analysis/find-entry analysis :vertex)]
      (vertex-varying-name-mapping v-struct (set (map :name (:params v-entry)))))
    {}))

;; ═══════════════════════════════════════════
;; sampler 规划
;; ═══════════════════════════════════════════

(defn- resolve-sample-entries
  [^SLAnalysis analysis]
  (let [textures (into {} (map (fn [^SLTexture t] [(:name t) t])
                               (:textures analysis)))
        samplers (into {} (map (fn [^SLSampler s] [(:name s) s])
                               (:samplers analysis)))]
    (mapv (fn [^SLSampleCall sc]
            (let [tex      (get textures (:texture sc))
                  smp-name (:sampler sc)
                  smp      (when smp-name
                             (get samplers smp-name))]
              (when-not tex
                (throw (err/compile-error
                         :gl-undeclared-texture
                         "sample-call references undeclared texture"
                         {:texture (:texture sc)})))
              (when (and smp-name (not smp))
                (throw (err/compile-error
                         :gl-undeclared-sampler
                         "sample-call references undeclared sampler"
                         {:sampler smp-name})))
              {:texture tex :sampler smp}))
          (:sample-calls analysis))))

(defn- reg-num
  [reg]
  (when reg
    (parse-long (subs (name reg) 1))))

(defn- sort-key
  [reg]
  [(if (nil? reg) 1 0) (or (reg-num reg) 0)])

(defn- make-sampler-uniform
  [entry unit naming-strategy multi?]
  (let [^SLTexture tex (:texture entry)
        ^SLSampler smp (:sampler entry)]
    (->SamplerUniform
      (lang/sampler-glsl-name naming-strategy
                              (:name tex) (some-> smp :name)
                              (boolean (and smp multi?)))
      (:name tex) (some-> smp :name)
      (:dimension tex)
      (lang/sampler-type-name (:dimension tex) (:element-type tex)
                              (boolean (and smp (:compare? smp))))
      unit)))

(defn- plan-samplers
  [^SLAnalysis analysis cfg]
  (let [entries          (resolve-sample-entries analysis)
        binding-strategy (config/texture-sampler-binding-strategy cfg)
        naming-strategy  (config/sampler-naming-strategy cfg)
        tex->smps (reduce (fn [m {:keys [texture sampler]}]
                            (if sampler
                              (update m (:name texture) (fnil conj #{}) (:name sampler))
                              m))
                          {} entries)
        smp->texs (reduce (fn [m {:keys [texture sampler]}]
                            (if sampler
                              (update m (:name sampler) (fnil conj #{}) (:name texture))
                              m))
                          {} entries)
        multi?    (fn [{:keys [texture sampler]}]
                    (case naming-strategy
                      :texture-sampler-simple
                      (> (count (get tex->smps (:name texture) #{})) 1)
                      :sampler-texture-simple
                      (boolean (and sampler
                                    (> (count (get smp->texs (:name sampler) #{})) 1)))
                      false))
        make      (fn [entry unit]
                    (make-sampler-uniform entry unit naming-strategy (multi? entry)))
        sampler-first-key (fn [{:keys [texture sampler]}]
                            [(sort-key (:register sampler))
                             (sort-key (:register texture))])
        texture-first-key (fn [{:keys [texture sampler]}]
                            [(sort-key (:register texture))
                             (sort-key (:register sampler))])]
    (case binding-strategy

      :sampler-first
      (mapv (fn [i e] (make e i)) (range) (sort-by sampler-first-key entries))

      :texture-first
      (mapv (fn [i e] (make e i)) (range) (sort-by texture-first-key entries))

      :mapping
      (let [used (volatile! #{})]
        (mapv (fn [{:keys [texture sampler] :as entry}]
                (let [unit (config/map-texture-sampler-binding
                             cfg (:register texture) (:register sampler))]
                  (when (contains? @used unit)
                    (throw (err/compile-error
                             :gl-duplicate-binding
                             "duplicate binding from mapping"
                             {:texture (:name texture)
                              :sampler (:name sampler)
                              :binding unit})))
                  (vswap! used conj unit)
                  (make entry unit)))
              entries))

      (throw (err/compile-error
               :gl-unknown-binding-strategy
               "unknown texture-sampler binding strategy"
               {:strategy binding-strategy})))))

;; ═══════════════════════════════════════════
;; 入口点规划
;; ═══════════════════════════════════════════

(defn- plan-vertex-inputs
  "顶点输入——全部走 vertex-input 空间的 location 分配。
   vertex 的 :position 是普通属性，不是内置变量——gl_Position
   是输出侧的映射，由 plan-vertex-return 处理。

   vertex-input 空间的 location 在所有 GLSL 版本都支持——不需要
   varying-location-enabled? 判断。"
  [params cfg]
  (let [sems (mapv :semantic params)
        locs (allocate-semantic-locations sems cfg
                                          #(config/vertex-semantic-location cfg %))]
    (mapv (fn [^SLParam p]
            (layout/binding-io (:name p) (:type p) (:semantic p)
                               :location (get locs (:semantic p))))
          params)))

(defn- plan-struct-fields-split
  "每个字段一个 IODecl，location 从 loc-map 查。
   loc-map 为 nil 时不输出 location——varying-location-enabled? 为
   false 时的 varying 场景——链接按名称匹配。
   var-name 从 name-map 查——nil 时用字段原名。"
  [stage struct-decl loc-map name-map]
  (mapv (fn [field]
          (let [sem  (semantic/shader-semantic field)
                type (:type field)
                name (:name field)]
            (if (lang/builtin-semantic? stage sem)
              (layout/builtin-io
                (lang/builtin-var-name stage sem)
                type sem)
              (let [loc      (when loc-map (get loc-map sem))
                    var-name (or (get name-map sem) name)]
                (when (and loc-map (nil? loc))
                  (throw (err/compile-error
                           :gl-no-location-for-field
                           "no location assigned for struct field"
                           {:field name :semantic sem :stage stage})))
                (layout/binding-io var-name type sem
                                   :location loc)))))
        (:members struct-decl)))

(defn- plan-struct-fields-block
  "内置字段 IODecl + 自定义字段打包成 InterfaceBlock。
   block 按块名 + 成员名匹配，不用 location。"
  [stage direction struct-decl cfg]
  (let [members     (:members struct-decl)
        builtin?    #(lang/builtin-semantic? stage (semantic/shader-semantic %))
        builtins    (filterv builtin? members)
        bindings    (filterv (complement builtin?) members)
        builtin-ios (mapv (fn [f]
                            (let [sem (semantic/shader-semantic f)]
                              (layout/builtin-io
                                (lang/builtin-var-name stage sem)
                                (:type f) sem)))
                          builtins)
        block       (when (seq bindings)
                      (layout/make-interface-block
                        (config/interface-block-naming cfg (:name struct-decl))
                        (config/interface-block-instance-naming
                          cfg (:name struct-decl) stage direction)
                        (mapv (fn [f]
                                (layout/make-interface-block-member
                                  (:name f) (:type f)
                                  (semantic/shader-semantic f)))
                              bindings)))]
    (into builtin-ios (when block [block]))))

(defn- plan-struct-fields
  [stage direction struct-decl cfg loc-map name-map]
  (when (empty? (:members struct-decl))
    (throw (err/compile-error
             :gl-plan-struct-fields-failed
             "struct has no members"
             {:struct (:name struct-decl)})))
  (case (config/out-struct-strategy cfg)
    :split           (plan-struct-fields-split stage struct-decl loc-map name-map)
    :interface-block (plan-struct-fields-block stage direction struct-decl cfg)
    (throw (err/compile-error
             :gl-unknown-out-struct-strategy
             "unknown out-struct-strategy"
             {:strategy (config/out-struct-strategy cfg)}))))

(defn- struct-decl-of-vertex-return-if-interface-block
  "若 vertex 用 :interface-block 策略返回 struct，返回该 struct 定义。
   否则 nil——fragment 输入走独立声明的常规路径。"
  [^SLAnalysis analysis cfg struct-names asts]
  (when (= :interface-block (config/out-struct-strategy cfg))
    (vertex-return-struct analysis struct-names asts)))

(defn- plan-fragment-inputs-from-block
  "vertex 用 block——fragment 也用同名 block，无需 location。"
  [params cfg v-struct]
  (let [block-name      (config/interface-block-naming cfg (:name v-struct))
        instance-name   (config/interface-block-instance-naming
                          cfg (:name v-struct) :fragment :in)
        binding-members (filterv
                          (complement
                            #(lang/builtin-semantic?
                               :fragment (semantic/shader-semantic %)))
                          (:members v-struct))
        block           (when (seq binding-members)
                          (layout/make-interface-block
                            block-name instance-name
                            (mapv (fn [f]
                                    (layout/make-interface-block-member
                                      (:name f) (:type f)
                                      (semantic/shader-semantic f)))
                                  binding-members)))
        block-sems      (set (map semantic/shader-semantic binding-members))
        builtin-ios     (into []
                              (keep (fn [^SLParam p]
                                      (let [sem (:semantic p)]
                                        (when (lang/builtin-semantic? :fragment sem)
                                          (layout/builtin-io
                                            (lang/builtin-var-name :fragment sem)
                                            (:type p) sem)))))
                              params)]
    (doseq [^SLParam p params
            :let [sem (:semantic p)]
            :when (and (not (lang/builtin-semantic? :fragment sem))
                       (not (contains? block-sems sem)))]
      (throw (err/compile-error
               :gl-fragment-semantic-missing-in-block
               "fragment param semantic missing in vertex struct"
               {:param (:name p) :semantic sem})))
    (into builtin-ios (when block [block]))))

(defn- fragment-input-semantics
  "展开 fragment 输入参数为 varying 空间的 semantic 列表。
   struct 类型参数展开其字段。"
  [params struct-names asts]
  (into []
        (mapcat (fn [^SLParam p]
                  (let [sem  (:semantic p)
                        type (:type p)]
                    (cond
                      (lang/builtin-semantic? :fragment sem) []
                      (hlsl.lang/struct-type? type struct-names)
                      (binding-semantics-of-struct
                        :fragment
                        (find-struct asts (ty/type-sym type)))
                      :else [sem]))))
        params))

(defn- plan-fragment-inputs-split
  "无 vertex block——varying 空间分配 location（如果支持）。"
  [params cfg struct-names asts analysis]
  (let [v-names  (vertex-varying-name-mapping-by-analysis analysis struct-names asts)
        all-sems (fragment-input-semantics params struct-names asts)
        locs     (when (config/varying-location-enabled? cfg)
                   (allocate-semantic-locations all-sems cfg
                                                #(config/varying-semantic-location cfg %)))]
    (into []
          (mapcat (fn [^SLParam p]
                    (let [sem  (:semantic p)
                          type (:type p)]
                      (cond
                        (lang/builtin-semantic? :fragment sem)
                        [(layout/builtin-io (lang/builtin-var-name :fragment sem)
                                            type sem)]

                        (hlsl.lang/struct-type? type struct-names)
                        (let [sd (find-struct asts (ty/type-sym type))]
                          (plan-struct-fields :fragment :in sd cfg locs v-names))

                        :else
                        (let [var-name (or (get v-names sem) (:name p))
                              loc      (when locs (get locs sem))]
                          [(layout/binding-io var-name type sem loc)])))))
          params)))

(defn- plan-fragment-inputs
  [params cfg struct-names asts analysis]
  (if-let [v-struct (struct-decl-of-vertex-return-if-interface-block
                      analysis cfg struct-names asts)]
    (plan-fragment-inputs-from-block params cfg v-struct)
    (plan-fragment-inputs-split params cfg struct-names asts analysis)))

(defn- plan-vertex-return
  "vertex 返回——:split 走 varying 空间 location 分配。

   varying-location-enabled? 为 false 时（GLSL 330 的 varying），
   locations 传 nil——plan-struct-fields 不输出 layout(location)。
   链接依靠名称匹配（fragment 侧查同一 varying 名映射）。

   varying 名和 params 名去冲突——加 _<inc> 后缀。struct 字段的
   :position 映射 gl_Position（builtin），其余字段是 varying out。"
  [entry cfg struct-names asts]
  (let [ret  (:return entry)
        type (:type ret)]
    (if (hlsl.lang/struct-type? type struct-names)
      (let [struct-decl (find-struct asts (ty/type-sym type))
            sems        (binding-semantics-of-struct :vertex struct-decl)
            ;; varying 空间绑定特性开关
            loc-map     (when (config/varying-location-enabled? cfg)
                          (allocate-semantic-locations
                            sems cfg
                            #(config/varying-semantic-location cfg %)))
            name-map    (vertex-varying-name-mapping
                          struct-decl
                          (set (map :name (:params entry))))]
        (plan-struct-fields :vertex :out struct-decl cfg loc-map name-map))
      (do
        ;; 非结构体类型一定为 position 语义
        ;; 没标注不检查，标注就检查冲突
        (when-let [sem (:semantic ret)]
          (when-not (= sem :position)
            (throw (err/compile-error
                     :gl-unexpected-semantic
                     "Unexpected vertex return semantic"
                     {:expected :position
                      :actual sem
                      :stage :vertex}))))
        [(layout/builtin-io "gl_Position" type :position)]))))

(defn- plan-fragment-return
  "fragment 返回——struct 走 fragment-output 空间 location 分配。

   fragment out 是 render target——所有 GLSL 版本都支持
   layout(location)——不需要 varying-location-enabled? 判断。
   不涉及 varying 名一致性——names 传 nil，用字段原名。
   单值 fragColor 硬编码 location 0；:depth 映射 gl_FragDepth。"
  [^SLReturn return cfg struct-names asts]
  (let [type (:type return)
        sem  (:semantic return)]
    (if (hlsl.lang/struct-type? type struct-names)
      (let [sd   (find-struct asts (ty/type-sym type))
            sems (binding-semantics-of-struct :fragment sd)
            locs (allocate-semantic-locations sems cfg
                                              #(config/fragment-semantic-location cfg %))]
        (plan-struct-fields :fragment :out sd cfg locs nil))
      (if (= sem :depth)
        ;; 单返回值时候，要么返回深度，要么返回 color-attachment
        [(layout/builtin-io "gl_FragDepth" type :depth)]
        [(layout/binding-io (config/fragment-out-name cfg)
                            type sem
                            :location 0)]))))

(defn- plan-graphics-entry
  [^SLGraphicsEntry entry cfg struct-names asts analysis]
  (let [stage  (:stage entry)
        params (:params entry)
        return (:return entry)]
    (case stage
      :vertex
      (layout/vertex-entry
        (plan-vertex-inputs params cfg)
        (plan-vertex-return entry cfg struct-names asts))

      :fragment
      (layout/fragment-entry
        (plan-fragment-inputs params cfg struct-names asts analysis)
        (plan-fragment-return return cfg struct-names asts))

      (throw (err/compile-error
               :gl-unsupported-stage
               "unsupported graphics entry stage"
               {:stage stage})))))

(defn- plan-entry
  [entry cfg struct-names asts analysis]
  (cond
    (instance? SLGraphicsEntry entry)
    (plan-graphics-entry entry cfg struct-names asts analysis)

    (instance? SLComputeEntry entry)
    (throw (err/compile-error
             :gl-compute-not-supported
             "compute entry-point not yet supported"
             {:name (:name entry)}))

    :else
    (throw (err/compile-error
             :gl-unknown-entry-type
             "unknown entry type"
             {:entry entry}))))

;; ═══════════════════════════════════════════
;; 主入口
;; ═══════════════════════════════════════════

(defn stage-keeps
  "当前 stage 需要强制保留的顶层符号集合。

   fragment + :interface-block 策略 + vertex 返回 struct 时——
   fragment 参数可能是离散的，不引用 vertex struct，liveness
   的引用分析抓不到它，会把它当死代码删掉；但
   plan-fragment-inputs 需要这个 struct 的字段定义来生成匹配的
   block。因此强制保留。

   其他情况返回空集——liveness 按引用分析决定保留。"
  [^SLAnalysis analysis stage cfg]
  (if (and (= stage :fragment)
           (= :interface-block (config/out-struct-strategy cfg)))
    (if-let [v-ty (vertex-return-type analysis)]
      (if-let [sym (ty/type-sym v-ty)] #{sym} #{})
      #{})
    #{}))

(defn plan
  "对 SLAnalysis 做语义降级规划。

   asts     —— 当前入口点组的 ShaderAST——struct 展开需要查 struct 定义
   analysis —— 全局分析结果，sampler 规划跨入口点共享；fragment 输入
               规划需要查 vertex 输出形态
   stage    —— 当前处理的着色阶段 :vertex / :fragment / :compute
   cfg      —— GLSL 配置

   返回 Layout——含全局 sampler 规划和当前 stage 的入口点规划。"
  ^Layout [asts ^SLAnalysis analysis stage cfg]
  (let [entry        (analysis/find-entry analysis stage)
        struct-names (collect-struct-names asts)]
    (->Layout
      (plan-samplers analysis cfg)
      (plan-entry entry cfg struct-names asts analysis))))
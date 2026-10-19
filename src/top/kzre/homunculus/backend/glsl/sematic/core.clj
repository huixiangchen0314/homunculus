(ns top.kzre.homunculus.backend.glsl.sematic.core
  "语义降级规划的 lower 实现。

   ── 职责 ──────────────────────────────────────────────
   根据 SLAnalysis + 当前 stage + 该入口点组的 asts，生成 GLSL
   降级所需的规划数据：
   ▸ sampler 规划——每个 (texture, sampler) 组合 → SamplerUniform，
     按 load-only 规则区分是否带 sampler
   ▸ 入口点规划——入口点的 params / return → in/out IODecl 或
     InterfaceBlock，按 out-struct-strategy 展开

   ── 语言层工具 ────────────────────────────────────────
   类型映射、sampler 类型推导、flat 判定、内置语义走 glsl.lang；
   struct 类型判定走 hlsl.lang。本层只负责规划编排。

   ── 不在本层做的事 ────────────────────────────────────
   ▸ uniform block 的 binding——逐节点翻译，尊重 register(bN)
   ▸ vertex input struct 参数——当前只处理离散参数
   ▸ 名字冲突检测——假设 HLSL 变量名与 GLSL 生成名不冲突"
  (:require
    [top.kzre.homunculus.backend.glsl.lang :as lang]
    [top.kzre.homunculus.backend.glsl.sematic.config :as config]
    [top.kzre.homunculus.backend.glsl.sematic.layout
     :refer [->EntryLayout ->IODecl ->InterfaceBlock
             ->InterfaceBlockMember ->Layout ->SamplerUniform]]
    [top.kzre.homunculus.backend.hlsl.lang :as hlsl.lang]
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.backend.shader.pass.analyze.model :as model]
    [top.kzre.homunculus.backend.shader.semantic :as semantic]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.core.error :as err])
  (:import
    (top.kzre.homunculus.backend.glsl.sematic.layout
      Layout)
    (top.kzre.homunculus.backend.shader.pass.analyze.model
      SLAnalysis SLComputeEntry SLGraphicsEntry SLParam SLReturn
      SLSampleCall SLSampler SLTexture)))

;; ═══════════════════════════════════════════
;; AST 辅助
;; ═══════════════════════════════════════════

(defn- collect-struct-names
  "收集 asts 里所有 :struct 声明的名字集合。"
  [asts]
  (into #{}
        (keep (fn [n]
                (when (= :struct (ast/kind n))
                  (:name n))))
        asts))

(defn- find-struct
  "从 asts 中查找 struct 定义。找不到时抛异常。"
  [asts struct-name]
  (or (some (fn [n]
              (when (and (= :struct (ast/kind n))
                         (= struct-name (:name n)))
                n))
            asts)
      (throw (ex-info "struct definition not found"
                      {:struct struct-name}))))

;; ═══════════════════════════════════════════
;; sampler 规划
;; ═══════════════════════════════════════════

(defn- resolve-sample-entries
  "把 sample-calls 关联到 texture / sampler 记录。
   引用未声明的 resource 时抛异常。

   load 的 sample-call 的 :sampler 为 nil——它只引用 texture，
   不引用 sampler（HLSL 的 tex.Load 不经过 sampler）。此类记录
   只校验 texture，sampler 留 nil 传递给 plan-samplers。"
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
                (throw (ex-info "sample-call references undeclared texture"
                                {:texture (:texture sc)})))
              (when (and smp-name (not smp))
                (throw (ex-info "sample-call references undeclared sampler"
                                {:sampler smp-name})))
              {:texture tex :sampler smp}))
          (:sample-calls analysis))))

(defn- reg-num
  "从寄存器关键字提取数字。:t0 → 0，:s3 → 3。nil → nil。"
  [reg]
  (when reg
    (parse-long (subs (name reg) 1))))

(defn- sort-key
  "排序键。有 register 的按数字排在前，nil 的排最后。"
  [reg]
  [(if (nil? reg) 1 0) (or (reg-num reg) 0)])

(defn- make-sampler-uniform
  [entry unit naming-strategy multi?]
  (let [^SLTexture tex (:texture entry)
        ^SLSampler smp (:sampler entry)]
    (if smp
      (->SamplerUniform
        (lang/sampler-glsl-name
          naming-strategy
          (:name tex) (:name smp) multi?)
        (:name tex) (:name smp)
        (:dimension tex)
        (lang/sampler-type-name (:dimension tex)
                                (:element-type tex)
                                (:compare? smp))
        unit)
      ;; load-only——独立 uniform，无 compare?
      (->SamplerUniform
        (lang/sampler-glsl-name
          naming-strategy
          (:name tex) nil false)
        (:name tex) nil
        (:dimension tex)
        (lang/sampler-type-name (:dimension tex)
                                (:element-type tex)
                                false)
        unit))))

(defn- plan-samplers
  "遍历 sample-calls，为每个 (texture, sampler) 组合生成 SamplerUniform。
   按 config 策略分配 texture unit。"
  [^SLAnalysis analysis cfg]
  (let [entries          (resolve-sample-entries analysis)
        binding-strategy (config/texture-sampler-binding-strategy cfg)
        naming-strategy  (config/sampler-naming-strategy cfg)
        ;; simple 策略的冲突统计——load-only（sampler 为 nil）不参与
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
                      (and sampler
                           (> (count (get smp->texs (:name sampler) #{})) 1))
                      false))
        make      (fn [entry unit]
                    (make-sampler-uniform entry unit naming-strategy (multi? entry)))]
    (case binding-strategy

      :sampler-first
      (let [sorted (sort-by (fn [{:keys [texture sampler]}]
                              [(sort-key (:register sampler))
                               (sort-key (:register texture))])
                            entries)]
        (mapv (fn [i e] (make e i)) (range) sorted))

      :texture-first
      (let [sorted (sort-by (fn [{:keys [texture sampler]}]
                              [(sort-key (:register texture))
                               (sort-key (:register sampler))])
                            entries)]
        (mapv (fn [i e] (make e i)) (range) sorted))

      :mapping
      (let [used (volatile! #{})]
        (mapv (fn [{:keys [texture sampler] :as entry}]
                (let [unit (config/map-texture-sampler-binding
                             cfg (:register texture) (:register sampler))]
                  (when (contains? @used unit)
                    (throw (ex-info "duplicate binding from mapping"
                                    {:texture (:name texture)
                                     :sampler (:name sampler)
                                     :binding unit})))
                  (vswap! used conj unit)
                  (make entry unit)))
              entries))

      (throw (ex-info "unknown texture-sampler binding strategy"
                      {:strategy binding-strategy})))))

;; ═══════════════════════════════════════════
;; 入口点规划
;; ═══════════════════════════════════════════

(defn- plan-vertex-inputs
  "顶点输入——按语义查 location。无法查到时抛异常。"
  [params cfg]
  (mapv (fn [^SLParam p]
          (let [sem      (:semantic p)
                location (config/vertex-semantic-location cfg sem)]
            (when (nil? location)
              (throw (ex-info "no location configured for vertex input"
                              {:param (:name p) :semantic sem})))
            (->IODecl (:name p) (:type p) sem location
                      (lang/flat-required? (:type p)) false)))
        params))

(defn- plan-struct-fields
  "展开 struct 的字段。

   :split           —— 每个字段一个 IODecl（内置字段标 builtin? true）。
   :interface-block —— 内置字段 IODecl + 自定义字段打包成一个 InterfaceBlock。"
  [stage struct-decl cfg direction]
  (when (empty? (:members struct-decl))
    (throw (err/compile-error
             :gl-plan-struct-fields-failed
             "struct has no members"
             {:struct (:name struct-decl)})))
  (let [strategy (config/out-struct-strategy cfg)]
    (case strategy
      :split
      (mapv (fn [field]
              (let [sem  (semantic/shader-semantic field)
                    type (:type field)
                    name (:name field)]
                (if (lang/builtin-semantic? stage sem)
                  (->IODecl (lang/builtin-var-name stage sem)
                            type sem nil
                            (lang/flat-required? type) true)
                  (let [location (case stage
                                   :vertex   (config/vertex-semantic-location cfg sem)
                                   :fragment (config/fragment-semantic-location cfg sem))]
                    (when (nil? location)
                      (throw (ex-info "no location configured for struct field"
                                      {:field name :semantic sem :stage stage})))
                    (->IODecl name type sem location
                              (lang/flat-required? type) false)))))
            (:members struct-decl))

      :interface-block
      (let [members     (:members struct-decl)
            is-builtin? (fn [f] (lang/builtin-semantic?
                                  stage (semantic/shader-semantic f)))
            builtins    (filterv is-builtin? members)
            customs     (filterv (complement is-builtin?) members)
            builtin-ios (mapv (fn [f]
                                (let [sem (semantic/shader-semantic f)]
                                  (->IODecl (lang/builtin-var-name stage sem)
                                            (:type f) sem nil
                                            (lang/flat-required? (:type f)) true)))
                              builtins)
            block       (when (seq customs)
                          (->InterfaceBlock
                            (config/interface-block-naming cfg (:name struct-decl))
                            (config/interface-block-instance-naming
                              cfg (:name struct-decl) stage direction)
                            nil
                            (mapv (fn [f]
                                    (->InterfaceBlockMember
                                      (:name f) (:type f) nil
                                      (lang/flat-required? (:type f))))
                                  customs)))]
        (into builtin-ios (when block [block])))

      (throw (ex-info "unknown out-struct-strategy" {:strategy strategy})))))

(defn- plan-fragment-inputs
  [params cfg struct-names asts]
  (into []
        (mapcat (fn [^SLParam p]
                  (let [sem  (:semantic p)
                        type (:type p)]
                    (cond
                      ;; 内置语义 → 内置变量
                      (lang/builtin-semantic? :fragment sem)
                      [(->IODecl (lang/builtin-var-name :fragment sem)
                                 type sem nil
                                 (lang/flat-required? type) true)]

                      ;; struct 参数 → plan-struct-fields 内部按策略分叉
                      ;;   :split           → 展开为离散 IODecl
                      ;;   :interface-block → block + 内置 IODecl
                      (hlsl.lang/struct-type? type struct-names)
                      (let [sd (find-struct asts (ty/type-sym type))]
                        (plan-struct-fields :fragment sd cfg :in))

                      ;; 离散参数 → 离散 IODecl（与策略无关）
                      :else
                      [(->IODecl (:name p) type sem nil
                                 (lang/flat-required? type) false)]))))
        params))

(defn- plan-vertex-return
  [^SLReturn return cfg struct-names asts]
  (let [type (:type return)]
    (if (hlsl.lang/struct-type? type struct-names)
      (let [sd (find-struct asts (ty/type-sym type))]
        (plan-struct-fields :vertex sd cfg :out))
      [(->IODecl "gl_Position" type :position nil
                 (lang/flat-required? type) true)])))

(defn- plan-fragment-return
  [^SLReturn return cfg struct-names asts]
  (let [type (:type return)
        sem  (:semantic return)]
    (if (hlsl.lang/struct-type? type struct-names)
      (let [sd (find-struct asts (ty/type-sym type))]
        (plan-struct-fields :fragment sd cfg :out))
      (if (= sem :depth)
        [(->IODecl "gl_FragDepth" type :depth nil
                   (lang/flat-required? type) true)]
        (let [location (or (config/fragment-semantic-location cfg sem) 0)]
          [(->IODecl (config/fragment-out-name cfg)
                     type sem location
                     (lang/flat-required? type)
                     false)])))))

(defn- plan-graphics-entry
  [^SLGraphicsEntry entry cfg struct-names asts]
  (let [stage  (:stage entry)
        params (:params entry)
        return (:return entry)]
    (case stage
      :vertex
      (->EntryLayout
        :vertex
        (plan-vertex-inputs params cfg)
        (plan-vertex-return return cfg struct-names asts))

      :fragment
      (->EntryLayout
        :fragment
        (plan-fragment-inputs params cfg struct-names asts)
        (plan-fragment-return return cfg struct-names asts))

      (throw (ex-info "unsupported graphics entry stage"
                      {:stage stage})))))

(defn- plan-entry
  [entry cfg struct-names asts]
  (cond
    (instance? SLGraphicsEntry entry)
    (plan-graphics-entry entry cfg struct-names asts)

    (instance? SLComputeEntry entry)
    (throw (ex-info "compute entry-point not yet supported"
                    {:name (:name entry)}))

    :else
    (throw (ex-info "unknown entry type" {:entry entry}))))

;; ═══════════════════════════════════════════
;; 主入口
;; ═══════════════════════════════════════════

(defn plan
  "对 SLAnalysis 做语义降级规划。

   asts     —— 当前入口点组的 ShaderAST——struct 展开需要查 struct 定义
   analysis —— 全局分析结果，sampler 规划跨入口点共享
   stage    —— 当前处理的着色阶段 :vertex / :fragment / :compute
   cfg      —— GLSL 配置

   返回 Layout——含全局 sampler 规划和当前 stage 的入口点规划。"
  ^Layout [asts ^SLAnalysis analysis stage cfg]
  (let [entry        (model/find-entry analysis stage)
        struct-names (collect-struct-names asts)]
    (->Layout
      (plan-samplers analysis cfg)
      (plan-entry entry cfg struct-names asts))))
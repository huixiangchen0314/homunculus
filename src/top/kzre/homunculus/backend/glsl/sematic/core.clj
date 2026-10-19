(ns top.kzre.homunculus.backend.glsl.sematic.core
  "语义降级规划的 lower 实现。

   ── 职责 ──────────────────────────────────────────────
   根据 SLAnalysis + 当前 stage + 该入口点组的 asts，生成 GLSL
   降级所需的规划数据：
   ▸ sampler 规划——每个 (texture, sampler) 组合 → SamplerUniform，
     按 load-only 规则区分是否带 sampler
   ▸ 入口点规划——入口点的 params / return → in/out IODecl 或
     InterfaceBlock，按 out-struct-strategy 展开

   ── 跨阶段联动 ────────────────────────────────────────
   vertex 用 :interface-block 策略返回 struct 时，fragment 输入必须
   用同名 block 接收（GLSL 链接规则）。即使 fragment 声明为离散参数，
   也生成对应的 block——非 builtin 参数不再生成独立 IODecl，
   wrapper 在 emit 阶段从 block instance 按 semantic 读成员。

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
  [asts]
  (into #{}
        (keep (fn [n]
                (when (= :struct (ast/kind n))
                  (:name n))))
        asts))

(defn- find-struct
  [asts struct-name]
  (or (some (fn [n]
              (when (and (= :struct (ast/kind n))
                         (= struct-name (:name n)))
                n))
            asts)
      (throw (ex-info "struct definition not found"
                      {:struct struct-name}))))

(defn- find-graphics-entry
  "从 analysis 中查找指定 stage 的图形入口点。找不到返回 nil。"
  [^SLAnalysis analysis stage]
  (some (fn [e]
          (when (and (instance? SLGraphicsEntry e)
                     (= stage (:stage e)))
            e))
        (:graphics-entries analysis)))

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
                (throw (ex-info "sample-call references undeclared texture"
                                {:texture (:texture sc)})))
              (when (and smp-name (not smp))
                (throw (ex-info "sample-call references undeclared sampler"
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
   :interface-block —— 内置字段 IODecl + 自定义字段打包成一个 InterfaceBlock。
   block 成员携带 semantic——跨阶段 wrapper 按语义匹配用。"
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
                                      (:name f) (:type f)
                                      (semantic/shader-semantic f)
                                      nil
                                      (lang/flat-required? (:type f))))
                                  customs)))]
        (into builtin-ios (when block [block])))

      (throw (ex-info "unknown out-struct-strategy" {:strategy strategy})))))

(defn- vertex-block-struct
  "若 vertex 用 :interface-block 策略返回 struct，返回该 struct 定义。
   否则 nil——fragment 输入走独立声明的常规路径。"
  [^SLAnalysis analysis cfg struct-names asts]
  (when (= :interface-block (config/out-struct-strategy cfg))
    (let [v-entry  (find-graphics-entry analysis :vertex)
          v-return (when v-entry (:return v-entry))
          v-ty     (when v-return (:type v-return))]
      (when (and v-ty (hlsl.lang/struct-type? v-ty struct-names))
        (find-struct asts (ty/type-sym v-ty))))))

(defn- plan-fragment-inputs
  [params cfg struct-names asts analysis]
  (if-let [v-struct (vertex-block-struct analysis cfg struct-names asts)]
    ;; ── vertex 用 block 输出——fragment 输入也必须是 block ──
    (let [block-name    (config/interface-block-naming cfg (:name v-struct))
          instance-name (config/interface-block-instance-naming
                          cfg (:name v-struct) :fragment :in)
          customs       (filterv
                          (complement
                            #(lang/builtin-semantic?
                               :fragment (semantic/shader-semantic %)))
                          (:members v-struct))
          block         (->InterfaceBlock
                          block-name instance-name nil
                          (mapv (fn [f]
                                  (->InterfaceBlockMember
                                    (:name f) (:type f)
                                    (semantic/shader-semantic f)
                                    nil
                                    (lang/flat-required? (:type f))))
                                customs))
          builtin-ios   (into []
                              (keep (fn [^SLParam p]
                                      (let [sem (:semantic p)]
                                        (when (lang/builtin-semantic? :fragment sem)
                                          (->IODecl (lang/builtin-var-name :fragment sem)
                                                    (:type p) sem nil
                                                    (lang/flat-required? (:type p))
                                                    true)))))
                              params)]
      ;; 校验：每个非 builtin 参数的 semantic 必须在 vertex struct 里有字段
      (doseq [^SLParam p params
              :let [sem (:semantic p)]
              :when (and (not (lang/builtin-semantic? :fragment sem))
                         (not-any? #(= sem (semantic/shader-semantic %)) customs))]
        (throw (ex-info "fragment param semantic missing in vertex struct"
                        {:param (:name p) :semantic sem})))
      (into builtin-ios [block]))

    ;; ── 无 vertex block——原逻辑 ──
    (into []
          (mapcat (fn [^SLParam p]
                    (let [sem  (:semantic p)
                          type (:type p)]
                      (cond
                        (lang/builtin-semantic? :fragment sem)
                        [(->IODecl (lang/builtin-var-name :fragment sem)
                                   type sem nil
                                   (lang/flat-required? type) true)]

                        (hlsl.lang/struct-type? type struct-names)
                        (let [sd (find-struct asts (ty/type-sym type))]
                          (plan-struct-fields :fragment sd cfg :in))

                        :else
                        [(->IODecl (:name p) type sem nil
                                   (lang/flat-required? type) false)]))))
          params)))

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
  [^SLGraphicsEntry entry cfg struct-names asts analysis]
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
        (plan-fragment-inputs params cfg struct-names asts analysis)
        (plan-fragment-return return cfg struct-names asts))

      (throw (ex-info "unsupported graphics entry stage"
                      {:stage stage})))))

(defn- plan-entry
  [entry cfg struct-names asts analysis]
  (cond
    (instance? SLGraphicsEntry entry)
    (plan-graphics-entry entry cfg struct-names asts analysis)

    (instance? SLComputeEntry entry)
    (throw (ex-info "compute entry-point not yet supported"
                    {:name (:name entry)}))

    :else
    (throw (ex-info "unknown entry type" {:entry entry}))))

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
    (let [v-entry (find-graphics-entry analysis :vertex)
          v-ty    (some-> v-entry :return :type)
          v-sym   (when v-ty (ty/type-sym v-ty))]
      (if v-sym #{v-sym} #{}))
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
  (let [entry        (model/find-entry analysis stage)
        struct-names (collect-struct-names asts)]
    (->Layout
      (plan-samplers analysis cfg)
      (plan-entry entry cfg struct-names asts analysis))))
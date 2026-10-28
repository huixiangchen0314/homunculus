(ns top.kzre.homunculus.frontend.shader.pass.analyze.model
  "分析阶段的数据模型。只承载「涉及语义重分配」的信息——
   逐节点结构翻译的部分（uniform、global-var、struct、cbuffer、
   CS 资源与调用、表达式、控制流）不在此模型中出现。

   本模型是源语言无关的语义层。环境（env）接收分析器已解构的
   字段，构造本模型的记录。

   ── 寄存器 ────────────────────────────────────────────
   寄存器用关键字表达：:t0 :s0 :b0 :u0。不拆成记录。

   ── 入口点 ────────────────────────────────────────────
   图形入口和计算入口分开建模——字段集不同：
     图形：stage + params + return
     计算：params + thread-group（无 return）

   返回值提取为 SLReturn 记录——type 和 semantic 是返回值的
   固有属性，独立成记录让类型意图显式化。

   ── 前端约束 ──────────────────────────────────────────
   前端是 Clojure DSL——表达式语言，每个函数有返回值。不存在
   void 返回值。SLGraphicsEntry.return 永远非 nil。")

;; ═══════════════════════════════════════════
;; 入口点
;; ═══════════════════════════════════════════

;; 参数。
;; semantic: :position :normal :texcoord0 :target0
;;           :sv-position :sv-target :sv-dispatch-thread-id ...
(defrecord SLParam
  [name
   type
   semantic])

;; 返回值。type 是 IR 类型对象；semantic 是语义关键字。
(defrecord SLReturn
  [type
   semantic])

;; CS 线程组配置
(defrecord SLThreadGroup
  [x y z])

;; 图形管线入口点。
;; stage  —— :vertex / :fragment
;; return —— SLReturn，永远非 nil
(defrecord SLGraphicsEntry
  [stage
   name
   params          ;; [SLParam ...]
   return])        ;; SLReturn

;; 计算着色器入口点。params 是系统值（SV_DispatchThreadID 等）。
(defrecord SLComputeEntry
  [name
   params          ;; [SLParam ...]
   thread-group])  ;; SLThreadGroup 或 nil

;; ═══════════════════════════════════════════
;; 纹理与采样
;; ═══════════════════════════════════════════

;; dimension:    :1d :2d :3d :cube :1d-array :2d-array :cube-array
;;               :2d-ms :2d-ms-array :buffer
;; element-type: IR 类型对象（如 (TCon 'float4)）
;; register:     关键字（:t0 :t1 ...）或 nil
(defrecord SLTexture
  [name
   register
   dimension
   element-type])

;; register:  关键字（:s0 :s1 ...）或 nil
;; compare?:  是否比较采样器——true 时降级为 shadow sampler
(defrecord SLSampler
  [name
   register
   compare?])

;; 采样调用配对。同一 texture 配多个 sampler 时降级器为每个
;; 组合生成独立 uniform。
(defrecord SLSampleCall
  [texture
   sampler])

;; ═══════════════════════════════════════════
;; 分析结果
;; ═══════════════════════════════════════════

(defrecord SLAnalysis
  [graphics-entries     ;; [SLGraphicsEntry ...]
   compute-entries      ;; [SLComputeEntry ...]
   textures             ;; [SLTexture ...]
   samplers             ;; [SLSampler ...]
   sample-calls])       ;; [SLSampleCall ...]


(defn find-entry
  "从 analysis 中找出指定 stage 的入口点。

   每个 stage 最多一个入口点——多个时抛异常。

   stage —— :vertex / :fragment / :compute

   返回 SLGraphicsEntry（:vertex/:fragment）或 SLComputeEntry（:compute）。"
  [^SLAnalysis analysis stage]
  (case stage
    (:vertex :fragment)
    (let [matches (filterv #(= stage (:stage %))
                           (:graphics-entries analysis))]
      (cond
        (empty? matches)
        (throw (ex-info "no entry-point for stage"
                        {:stage stage}))

        (> (count matches) 1)
        (throw (ex-info "multiple entry-points for same stage"
                        {:stage stage
                         :names (mapv :name matches)}))

        :else
        (first matches)))

    :compute
    (let [matches (:compute-entries analysis)]
      (cond
        (empty? matches)
        (throw (ex-info "no compute entry-point" {}))

        (> (count matches) 1)
        (throw (ex-info "multiple compute entry-points"
                        {:names (mapv :name matches)}))

        :else
        (first matches)))

    (throw (ex-info "unsupported stage" {:stage stage}))))
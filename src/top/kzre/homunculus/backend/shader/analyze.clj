(ns top.kzre.homunculus.backend.shader.analyze)

;; ═══════════════════════════════════════════
;; 基础
;; ═══════════════════════════════════════════

;; 寄存器，:t0 :s0 :b0 :u0
(defrecord SLRegister [kind index])

;; 支持的 stage：只做 VS / FS / CS
(def ^:const supported-stages #{:vertex :fragment :compute})

;; ═══════════════════════════════════════════
;; 入口点
;; ═══════════════════════════════════════════

;; 参数 / 返回
;; semantic: :position :normal :texcoord0 :target0
;;           :sv-position :sv-target :sv-dispatch-thread-id ...
(defrecord SLParam
  [name
   type
   semantic])

;; CS 线程组配置，图形管线为 nil
(defrecord SLThreadGroup
  [x y z])

;; 入口点
;; stage:    :vertex / :fragment / :compute
;; ret:      SLParam 或 nil（CS 无返回）
(defrecord SLEntry
  [stage
   name
   params
   ret
   thread-group])

;; ═══════════════════════════════════════════
;; 纹理与采样
;; ═══════════════════════════════════════════

;; dimension:    :1d :2d :3d :cube :1d-array :2d-array :cube-array
;;               :2d-ms :2d-ms-array :buffer
;; sampled-type: :float :int :uint :depth
(defrecord SLTexture
  [name
   register
   dimension
   sampled-type])

;; compare?: 是否比较采样器（HLSL ComparisonSampler）
(defrecord SLSampler
  [name
   register
   compare?])

;; 采样 / 访问点
;; kind:    :sample :load :fetch :gather :compare
;; sampler: nil 表示不走 sampler（Load / texelFetch）
(defrecord SLSampleUse
  [texture
   sampler
   kind
   coords])

;; ═══════════════════════════════════════════
;; 缓冲与 uniform
;; ═══════════════════════════════════════════

(defrecord SLCBufferMember
  [name type])

(defrecord SLCBuffer
  [name register members])

(defrecord SLUniform
  [name type])

(defrecord SLStatic
  [name init])

;; ═══════════════════════════════════════════
;; 结构化类型
;; ═══════════════════════════════════════════

(defrecord SLStructField
  [name type semantic])

(defrecord SLStructDef
  [name fields])

;; ═══════════════════════════════════════════
;; 计算着色器专用
;; ═══════════════════════════════════════════

;; 共享内存变量
(defrecord SLSharedVar
  [name type array-size])

;; 屏障调用
;; kind: :group :device :all
(defrecord SLBarrier
  [kind])

;; 原子调用
;; op:     :add :sub :min :max :and :or :xor :exchange :compare-exchange
;; result: :none :old :new（HLSL out 参数有无）
(defrecord SLAtomicUse
  [op target args result])

;; 读写资源
;; kind:      :structured :byte-address :rw-texture
;; access:    :read :write :readwrite
;; dimension: 对 :rw-texture 有效
(defrecord SLRWResource
  [name register kind access dimension element-type])

;; ═══════════════════════════════════════════
;; 分析结果
;; ═══════════════════════════════════════════

(defrecord SLAnalysis
  [entries        ;; [SLEntry ...]
   textures       ;; [SLTexture ...]
   samplers       ;; [SLSampler ...]
   sample-uses    ;; [SLSampleUse ...]
   cbuffers       ;; [SLCBuffer ...]
   uniforms       ;; [SLUniform ...]
   statics        ;; [SLStatic ...]
   structs        ;; [SLStructDef ...]
   shared-vars    ;; [SLSharedVar ...]      CS 专用
   barriers       ;; [SLBarrier ...]        CS 专用
   atomic-uses    ;; [SLAtomicUse ...]      CS 专用
   rw-resources]) ;; [SLRWResource ...]     CS 专用


(defn analyze
  "分析 shader-ast 返回 SLAnalysis 分析结果"
  [asts])
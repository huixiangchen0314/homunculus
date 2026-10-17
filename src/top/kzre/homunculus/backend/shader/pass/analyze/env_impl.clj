(ns top.kzre.homunculus.backend.shader.pass.analyze.env-impl
  "IEnv 协议的实现。单遍遍历期间逐步累积分析状态。

   env 是纯累积器——接收分析器已解析的字段，构造 SL 记录。
   不做 AST 解析、不做类型判断、不做兼容转换。

   register 字段只接受关键字（:t0 :s1 :b2 :u3）或 nil。
   element-type / return-type 存 IR 类型对象——不转关键字、不转字符串。
   param-v 是 [[name type semantic] ...] 三元组向量，由分析器从
   HLSL AST 参数节点解构好后传入。"
  (:require
    [top.kzre.homunculus.backend.shader.pass.analyze.env :refer [IEnv]]
    [top.kzre.homunculus.backend.shader.pass.analyze.model
     :refer [->SLAnalysis ->SLTexture ->SLSampler ->SLSampleCall
             ->SLGraphicsEntry ->SLComputeEntry ->SLParam
             ->SLReturn ->SLThreadGroup]]))

;; ═══════════════════════════════════════════
;; 数据构造辅助
;; ═══════════════════════════════════════════

(defn- triple->sl-param
  "把 [name type semantic] 三元组转为 SLParam。"
  [[n t s]]
  (->SLParam n t s))

;; ═══════════════════════════════════════════
;; Env 记录
;; ═══════════════════════════════════════════

(defrecord Env
  [graphics-entries
   compute-entries
   textures
   samplers
   sample-calls]

  IEnv

  ;; ── 纹理与采样器（结构降级） ───────────────

  (add-texture [this var-name register dimension element-type]
    (update this :textures conj
            (->SLTexture var-name register dimension element-type)))

  (add-sampler [this var-name register compare?]
    (update this :samplers conj
            (->SLSampler var-name register compare?)))

  (add-sample-call [this texture-name sampler-name]
    (let [already? (some (fn [sc]
                           (and (= (:texture sc) texture-name)
                                (= (:sampler sc) sampler-name)))
                         sample-calls)]
      (if already?
        this
        (update this :sample-calls conj
                (->SLSampleCall texture-name sampler-name)))))

  ;; ── 入口点（语义降级） ────────────────────

  (add-entry [this stage entry-name return-type return-semantic param-v]
    (update this :graphics-entries conj
            (->SLGraphicsEntry stage entry-name
                               (mapv triple->sl-param param-v)
                               (->SLReturn return-type return-semantic))))

  (cs-add-entry [this entry-name param-v thread-group]
    (let [sl-tg (when thread-group
                  (->SLThreadGroup (:x thread-group)
                                   (:y thread-group)
                                   (:z thread-group)))]
      (update this :compute-entries conj
              (->SLComputeEntry entry-name
                                (mapv triple->sl-param param-v)
                                sl-tg))))

  ;; ── 输出 ──────────────────────────────────

  (to-analysis [_]
    (->SLAnalysis graphics-entries
                  compute-entries
                  textures
                  samplers
                  sample-calls)))

;; ═══════════════════════════════════════════
;; 构造函数
;; ═══════════════════════════════════════════

(defn make-env
  "创建空环境。"
  []
  (->Env [] [] [] [] []))
(ns top.kzre.homunculus.backend.glsl.emitter
  (:require
   [top.kzre.homunculus.backend.glsl.config :as cfg]
   [top.kzre.homunculus.backend.glsl.emit.api :as emit]
   [top.kzre.homunculus.backend.glsl.lower.api :as glsl.lower]
   [top.kzre.homunculus.backend.glsl.semantic.api :as sematic]
   [top.kzre.homunculus.backend.shader.lower :as lower]
   [top.kzre.homunculus.backend.shader.pass.analyze.core :as analyze]
   [top.kzre.homunculus.backend.shader.pass.analyze.env :as env]
   [top.kzre.homunculus.backend.shader.pass.analyze.env-impl :as env-impl]
   [top.kzre.homunculus.backend.shader.pass.array-assign :as array-assign]
   [top.kzre.homunculus.backend.shader.pass.liveness :as liveness]
   [top.kzre.homunculus.backend.shader.pass.loopfor.api :as loopfor]
   [top.kzre.homunculus.backend.shader.pass.ns-strategy :as ns-strategy]
   [top.kzre.homunculus.backend.shader.pass.split-entries :as split-entries]
   [top.kzre.homunculus.backend.shader.semantic :as semantic]
   [top.kzre.homunculus.core.cfg.irstmt.adapt :as cfg-adapt]
   [top.kzre.homunculus.core.cfg.irstmt.opt :as opt]
   [top.kzre.homunculus.core.error :as err]
   [top.kzre.homunculus.core.irstmt.lower :as irstmt.lower]
   [top.kzre.homunculus.core.irstmt.pass.statementize :as statementize]
   [top.kzre.homunculus.internal.compile-result :as compile-result]
   [top.kzre.homunculus.internal.protocol :as ip]))

;; ═══════════════════════════════════════════
;; 路径生成
;; ═══════════════════════════════════════════

(defn- stage->extension
  [stage]
  (case stage
    :vertex   ".vert"
    :fragment ".frag"
    :compute  ".comp"
    (throw (err/compile-error :unknown-shader-stage
                              (str "Unknown shader stage: " stage)))))

;; ═══════════════════════════════════════════
;; 单个入口点的发射
;; ═══════════════════════════════════════════

(defn- emit-entry
  "为单个入口点生成 GLSL 源文件。
   返回 module-result。

   :asts 已经包含该入口点及其共享声明。emit/emit 接收 asts 列表，
   生成完整的 GLSL 源文本（含 #version prologue）。"
  [ctx path {:keys [entry asts]} analysis]
  (let [stage (semantic/shader-stage entry)
        path' (str path
                  (stage->extension stage))
        sem-config (sematic/make-config ctx)
        lived (-> asts
                 (liveness/analyze)
                 (liveness/elim :keeps (sematic/stage-keeps analysis stage sem-config)))
        layout (sematic/plan lived analysis stage
                             sem-config)
        lowered (glsl.lower/lower lived (glsl.lower/make-env ctx) layout)
        code  (emit/emit lowered (emit/make-env ctx) analysis)]
    (compile-result/make-module-result path' code)))

;; ═══════════════════════════════════════════
;; Emitter
;; ═══════════════════════════════════════════

(defrecord GLSLEmitter []
  ip/IEmitter
  (emit [_ nodes ctx {:keys [unit]}]
    (let [cfg (ip/config ctx)
          path (ip/output-path  cfg)
          ;; IR2 → IRStmt → 优化 → ShaderAST
          irstmts      (irstmt.lower/lower nodes (irstmt.lower/make-env))
          sted         (statementize/statementize-nodes irstmts)
          cfged        (opt/optimize sted (cfg-adapt/make-adapter ctx))
          shader-asts  (lower/lower cfged)
          fored        (loopfor/run shader-asts (loopfor/make-env ctx))
          vec-copied   (array-assign/expand fored)
          renamed (ns-strategy/apply vec-copied (cfg/ns-strategy (ip/config ctx)))
          ;; 分析 → 分流
          {:keys [env]} (analyze/analyze renamed (env-impl/make-env))
          analysis     (env/to-analysis env)
          groups       (split-entries/split renamed)]
      (if (= 1 (count groups))
        ;; 单入口——返回单个 module-result
        (emit-entry ctx path (first groups) analysis)
        ;; 多入口——返回 module-result 向量
        (mapv #(emit-entry ctx path % analysis) groups)))))

(defonce ^:private emitter* (->GLSLEmitter))

(defn emitter [] emitter*)
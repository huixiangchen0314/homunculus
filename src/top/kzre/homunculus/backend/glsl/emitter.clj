(ns top.kzre.homunculus.backend.glsl.emitter
  (:require
    [top.kzre.homunculus.backend.glsl.emit :as emit]
    [top.kzre.homunculus.backend.shader.lower :as lower]
    [top.kzre.homunculus.backend.shader.pass.analyze.core :as analyze]
    [top.kzre.homunculus.backend.shader.pass.array-assign :as array-assign]
    [top.kzre.homunculus.backend.shader.pass.split-entries.core :as split-entries]
    [top.kzre.homunculus.backend.shader.semantic :as semantic]
    [top.kzre.homunculus.core.irstmt.lower :as irstmt.lower]
    [top.kzre.homunculus.core.irstmt.pass.assign-propagate :as irstmt.assign-propagate]
    [top.kzre.homunculus.core.irstmt.pass.dce :as irstmt.dce]
    [top.kzre.homunculus.core.irstmt.pass.statementize :as statementize]
    [top.kzre.homunculus.internal.model :as m]
    [top.kzre.homunculus.internal.module-unit :as mu]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.utils :as iu]
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.backend.shader.pass.analyze.env-impl :as env-impl]
    [top.kzre.homunculus.backend.shader.pass.analyze.env :as env]))

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

(defn- unit-path
  [config unit stage]
  (let [out-dir (p/output-dir config)]
    (str out-dir "/"
         (iu/ns->module-path (mu/module-ns unit)
                             (p/module-naming-style config)
                             (stage->extension stage)))))

;; ═══════════════════════════════════════════
;; 单个入口点的发射
;; ═══════════════════════════════════════════

(defn- emit-entry
  "为单个入口点生成 GLSL 源文件。
   返回 module-result。

   :asts 已经包含该入口点及其共享声明。emit/emit 接收 asts 列表，
   生成完整的 GLSL 源文本（含 #version prologue）。"
  [config unit {:keys [entry asts]} analysis]
  (let [stage (semantic/shader-stage entry)
        path  (unit-path config unit stage)
        code  (emit/emit asts analysis)]
    (m/make-module-result path code)))

;; ═══════════════════════════════════════════
;; Emitter
;; ═══════════════════════════════════════════

(defrecord GLSLEmitter []
  p/IEmitter
  (emit [_ nodes ctx {:keys [unit]}]
    (let [config       (p/config ctx)
          ;; IR2 → IRStmt → 优化 → ShaderAST
          irstmts      (irstmt.lower/lower nodes)
          sted         (statementize/statementize-nodes irstmts)
          ped          (irstmt.assign-propagate/propagate-nodes sted)
          dced         (irstmt.dce/elim-nodes ped ctx)
          shader-asts  (lower/lower dced)
          vec-copied   (array-assign/expand shader-asts)

          ;; 分析 → 分流
          init-env     (env-impl/make-env)
          {:keys [env]} (analyze/analyze vec-copied init-env)
          analysis     (env/to-analysis env)
          groups       (split-entries/split vec-copied)]

      (when (empty? groups)
        (throw (err/compile-error :no-entry-point
                                  "GLSL shader must have one or more entry points.")))

      (if (= 1 (count groups))
        ;; 单入口——返回单个 module-result
        (emit-entry config unit (first groups) analysis)
        ;; 多入口——返回 module-result 向量
        (mapv #(emit-entry config unit % analysis) groups)))))

(defonce ^:private emitter* (->GLSLEmitter))

(defn emitter [] emitter*)
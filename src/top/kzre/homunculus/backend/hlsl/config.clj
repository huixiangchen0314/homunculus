(ns top.kzre.homunculus.backend.hlsl.config
  (:require
    [top.kzre.homunculus.backend.hlsl.api :as emit]
    [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
    [top.kzre.homunculus.backend.hlsl.folder :as folder]
    [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
    [top.kzre.homunculus.core.ir1.api :as ir1]
    [top.kzre.homunculus.core.ir2.api :as ir2]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.alias :as alias]
    [top.kzre.homunculus.core.types.alpha-rename :as rename]
    [top.kzre.homunculus.core.types.check.api :as check]
    [top.kzre.homunculus.core.types.constraint.api :as solve]
    [top.kzre.homunculus.core.types.dc-elim.core :as dce]
    [top.kzre.homunculus.core.types.fold.core :as fold]
    [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
    [top.kzre.homunculus.core.types.infer.api :as infer]
    [top.kzre.homunculus.core.types.inline.api :as inline]
    [top.kzre.homunculus.core.types.lambda-elim.api :as lambda-elim]
    [top.kzre.homunculus.core.types.lambda-elim.protocol :as lambda-elim-p]
    [top.kzre.homunculus.core.types.module.api :as module]
    [top.kzre.homunculus.core.types.mutability.core :as mut]
    [top.kzre.homunculus.core.types.recur-elim.api :as recur]
    [top.kzre.homunculus.internal.model :as model]
    [top.kzre.homunculus.internal.module-unit :as mu]
    [top.kzre.homunculus.internal.protocol :as p]))

;; ── 闭包消除配置 ──────────────────────
(defn- default-lift-config []
  (reify lambda-elim-p/ILiftConfig
    (max-iterations [_] 1000)
    (strict-mode? [_] true)
    (on-unresolved [_ lambda _reason]
      (throw (ex-info "Unresolved closure" {:lambda lambda})))
    (lift-name-gen [_ _lambda]
      ;; 生成唯一的提升函数名
      (symbol (str "lifted_" (gensym "lambda"))))))

(defrecord HLSLCompiler []
  p/ICompiler

  (emit [_this unit context]
    (let [roots     (:ir2-roots unit)
          frontend  (hlsl-front/->HLSLFrontend)
          backend   (hlsl-backend/->HLSLBackend)
          dce-ctx (dce/make-context context)                ;; 这些是必须在HLSL 消除的代码
          roots (dce/eliminate-ho-defs roots dce-ctx)
          roots (dce/eliminate-inline-defs roots dce-ctx)
          roots (dce/eliminate-polymorphic-defs roots dce-ctx)

          checked   (check/check roots (check/make-context context frontend backend))
          result    (emit/emit checked (emit/make-context context frontend))]
      result))

  ;; 模块编译
  (compile-module [_this forms context]
    (let [frontend  (hlsl-front/->HLSLFrontend)
          backend   (hlsl-backend/->HLSLBackend)
          lift-cfg  (default-lift-config)
          ns-sym    (some-> (first forms) (nth 1))
          _         (when (nil? ns-sym)
                      (throw (ex-info "No ns form found" {:forms forms})))
          processed (ir1/preprocess forms)
          ir1-roots (mapv ir1/->ir1 processed)
          ir2-roots (mapcat ir2/->ir2 ir1-roots)
          ir2-roots' (mapv rename/rename ir2-roots)
          ir2-roots' (alias/apply-alias ir2-roots' context frontend)
          ir2-roots' (module/resolve-ns ir2-roots' context frontend)
          _          (module/collect-symbols ir2-roots' context)
          ir2-roots' (inline/analyze ir2-roots')   ;; 分析标记
          ir2-roots' (inline/process ir2-roots' (inline/make-context context frontend backend))  ;; 执行内联
          no-ho    (ho-elim/process ir2-roots' (ho-elim/make-context context frontend backend))
          no-closure (lambda-elim/eliminate no-ho lift-cfg)
          no-recur   (mapv recur/eliminate no-closure)
          folded     (fold/fold no-recur (fold/make-context context frontend backend (folder/folder)))
          inferred   (infer/infer folded (infer/make-context context frontend backend))
          solved     (solve/process inferred (solve/make-context context frontend backend))
          mutable    (mut/analyze solved)
          _          (module/collect-symbols mutable context)
          unit       (mu/->ModuleUnit ns-sym mutable)]
      (model/set-module-unit! context ns-sym unit)
      unit))


  ;; 全局链接
  (link [_this context]
    (let [all-units (vals (get-in @(:state context) [:modules]))
          all-roots (mapcat :ir2-roots all-units)
          roots (remove n/ns-node? all-roots)
          ;; dce
          dce-ctx (dce/make-context context)   ;; 使用默认配置
          roots (dce/eliminate-ho-defs roots dce-ctx)
          roots (dce/eliminate-inline-defs roots dce-ctx)
          roots (dce/eliminate-polymorphic-defs roots dce-ctx)
          ;; 最终类型检查
          frontend  (hlsl-front/->HLSLFrontend)
          backend   (hlsl-backend/->HLSLBackend)
          checked   (check/check roots (check/make-context context frontend backend))
          ;; 代码生成
          result    (emit/emit checked (emit/make-context context frontend))]
      result)))
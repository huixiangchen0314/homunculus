(ns top.kzre.homunculus.backend.hlsl.config
  (:require
   [top.kzre.homunculus.backend.hlsl.api :as emit]
   [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
   [top.kzre.homunculus.backend.hlsl.folder :as folder]
   [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
   [top.kzre.homunculus.core.ir1.api :as ir1]
   [top.kzre.homunculus.core.ir2.api :as ir2]
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.core.types.alpha-rename :as rename]
   [top.kzre.homunculus.core.types.check.api :as check]
   [top.kzre.homunculus.core.types.constraint.api :as solve]
   [top.kzre.homunculus.core.types.dc-elim.core :as dce]
   [top.kzre.homunculus.core.types.fold.core :as fold]
   [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
   [top.kzre.homunculus.core.types.infer.api :as infer]
   [top.kzre.homunculus.core.types.lambda-elim.api :as lambda-elim]
   [top.kzre.homunculus.core.types.lambda-elim.protocol :as lambda-elim-p]
   [top.kzre.homunculus.core.types.module.api :as module]
   [top.kzre.homunculus.core.types.mutability.core :as mut]
   [top.kzre.homunculus.core.types.recur-elim.api :as recur]
   [top.kzre.homunculus.internal.model :as model]
   [top.kzre.homunculus.internal.module-unit :as mu]
   [top.kzre.homunculus.internal.protocol :as p]
   [top.kzre.homunculus.internal.utils :as u]))

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

  ;; 单文件便捷编译（保留现有逻辑，或调用模块化方法）
  (emit [_this forms context]
    (let [ns-sym (-> forms first second)]  ;; 获取 ns 表单的命名空间
      (p/compile-module _this ns-sym context)
      (p/link _this context)))

  ;; 模块编译
  (compile-module [_this ns-sym context]
    (let [lib-paths (p/lib-paths (p/config context))
          src       (u/resolve-module lib-paths ns-sym)]
      (when-not src
        (throw (ex-info "Module not found" {:module ns-sym :paths lib-paths})))
      (let [forms     (u/parse-forms src)
            frontend  (hlsl-front/->HLSLFrontend)
            backend   (hlsl-backend/->HLSLBackend)
            lift-cfg  (default-lift-config)

            processed (ir1/preprocess forms)
            ir1-roots (mapv ir1/->ir1 processed)
            ir2-roots (mapcat ir2/->ir2 ir1-roots)
            ir2-roots' (mapv rename/rename ir2-roots)
            ir2-roots' (module/resolve-ns ir2-roots' context frontend)
            _          (module/collect-symbols ir2-roots' context)
            inlined    (ho-elim/process ir2-roots' (ho-elim/make-context context frontend backend))
            no-closure (lambda-elim/eliminate inlined lift-cfg)
            no-recur   (mapv recur/eliminate no-closure)
            folded     (fold/fold no-recur (fold/make-context context frontend backend (folder/folder)))
            inferred   (infer/infer folded (infer/make-context context frontend backend))
            solved     (solve/process inferred (solve/make-context context frontend backend))
            mutable    (mut/analyze solved)
            _          (module/collect-symbols mutable context)
            unit       (mu/->ModuleUnit ns-sym mutable)]
        (model/set-module-unit! context ns-sym unit)
        unit)))


  ;; 全局链接
  (link [_this context]
    (let [all-units (vals (get-in @(:state context) [:modules]))
          all-roots (mapcat :ir2-roots all-units)
          roots (if (= 1 (count all-units))
                  (remove n/ns-node? all-roots)
                  all-roots)
          ;; 全局 DCE（暂用现有函数，或跳过）
          no-dead   (dce/eliminate-ho-defs roots context)
          ;; 最终类型检查
          frontend  (hlsl-front/->HLSLFrontend)
          backend   (hlsl-backend/->HLSLBackend)
          checked   (check/check no-dead (check/make-context context frontend backend))
          ;; 代码生成
          result    (emit/emit checked (emit/make-context context frontend))]
      result)))
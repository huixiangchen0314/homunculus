
(ns top.kzre.homunculus.compilers.firstorder
(:require
 [top.kzre.homunculus.core.ir1.api :as ir1]
 [top.kzre.homunculus.core.ir1.polyfill-meta :as pm]
 [top.kzre.homunculus.core.ir2.api :as ir2]
 [top.kzre.homunculus.core.ir2.node :as n]
 [top.kzre.homunculus.core.types.alias :as alias]
 [top.kzre.homunculus.core.types.alpha-rename :as rename]
 [top.kzre.homunculus.core.types.check.api :as check]
 [top.kzre.homunculus.core.types.constraint.api :as solve]
 [top.kzre.homunculus.core.types.dc-elim.analyze :as dce-analyze]
 [top.kzre.homunculus.core.types.dc-elim.assign-propagate :as assign-propagate]
 [top.kzre.homunculus.core.types.dc-elim.core :as dce]
 [top.kzre.homunculus.core.types.fold.core :as fold]
 [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
 [top.kzre.homunculus.core.types.infer.api :as infer]
 [top.kzre.homunculus.core.types.inline.api :as inline]
 [top.kzre.homunculus.core.types.lambda-elim.api :as lambda-elim]
 [top.kzre.homunculus.core.types.lambda-elim.protocol :as lambda-elim-p]
 [top.kzre.homunculus.core.types.module.api :as module]
 [top.kzre.homunculus.core.types.protocol :as tp]
 [top.kzre.homunculus.core.types.recur-elim.api :as recur]
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


(defrecord TypedCompiler []
  p/ICompiler
  ;; 模块编译
  (compile [_ forms ctx]
    (let [frontend (p/frontend ctx)
          backend (p/backend ctx)
          folder (tp/folder backend)

          lift-cfg  (default-lift-config)
          ns-sym    (some-> (first forms) (nth 1))
          _         (when (nil? ns-sym)
                      (throw (ex-info "No ns form found" {:forms forms})))
          unit (mu/make-module-unit ns-sym)
          processed (ir1/preprocess forms)
          ir1-roots (mapv ir1/->ir1 processed)
          ir1-roots1 (pm/polyfill-nodes ir1-roots)
          ir2-roots (ir2/lower-nodes ir1-roots1 ctx)
          ir2-roots' (rename/rename-nodes ir2-roots)
          ir2-roots' (alias/alias-nodes ir2-roots' ctx frontend)
          ir2-roots' (module/resolve-ns ir2-roots' ctx frontend)
          unit1      (module/collect-symbols ir2-roots' ctx unit)
          ir2-roots' (inline/analyze ir2-roots')   ;; 分析标记
          ir2-roots' (inline/inline-nodes ir2-roots' (inline/make-context ctx frontend backend))  ;; 执行内联
          no-ho      (ho-elim/eliminate ir2-roots' (ho-elim/make-context ctx frontend backend))
          no-closure (lambda-elim/eliminate no-ho lift-cfg)
          no-recur   (recur/eliminate no-closure)
          assigned   (assign-propagate/propagate-nodes no-recur ctx)
          folded     (fold/fold assigned (fold/make-context ctx frontend backend folder))
          inferred   (infer/infer folded (infer/make-context ctx frontend backend))
          solved     (solve/process inferred (solve/make-context ctx frontend backend))
          ;mutable    (mut/analyze solved)
          unit2      (module/collect-symbols solved ctx unit1)
          unit3      (assoc unit2 :nodes solved)]
      (p/set-module-unit! ctx unit3)
      unit3))

  (compile-module [_ unit context]
    (let [frontend (p/frontend context)
          backend (p/backend context)
          roots     (mu/module-nodes unit)
          dce-ctx (dce/make-context context)                ;; 这些是必须在HLSL 消除的代码
          roots (dce/eliminate-ho-defs roots dce-ctx)
          roots (dce/eliminate-inline-defs roots dce-ctx)
          roots (dce/eliminate-polymorphic-defs roots dce-ctx)
          roots (dce-analyze/analyze-nodes roots context)
          emitter (p/emitter context)
          checked   (check/check roots (check/make-context context frontend backend))
          result    (p/emit emitter checked context)]
      result))

  ;; 全局链接
  (link [_ context]
    (let [all-units (p/all-module-units context)
          all-roots (mapcat mu/module-nodes all-units)
          emitter (p/emitter context)
          roots (remove n/ns-node? all-roots)
          ;; dce
          dce-ctx (dce/make-context context)   ;; 使用默认配置
          roots (dce/eliminate-ho-defs roots dce-ctx)
          roots (dce/eliminate-inline-defs roots dce-ctx)
          roots (dce/eliminate-polymorphic-defs roots dce-ctx)
          ;; 最终类型检查
          frontend  (p/frontend context)
          backend   (p/backend context)
          checked   (check/check roots (check/make-context context frontend backend))
          ;; 代码生成
          result    (p/emit emitter checked context)]
      result)))

(defonce compiler (->TypedCompiler))
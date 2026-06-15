(ns top.kzre.homunculus.backend.hlsl.config
  "HLSL 后端的 ICompiler 实现，复用现有 Pass 管线。"
  (:require
   [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
   [top.kzre.homunculus.backend.shader.emit :as emit]
   [top.kzre.homunculus.core.ir1.core :as ir1]
   [top.kzre.homunculus.core.ir2.core :as ir2]
   [top.kzre.homunculus.core.types.builtin-check.core :as builtin]
   [top.kzre.homunculus.core.types.check.core :as check]
   [top.kzre.homunculus.core.types.elaborate.core :as elaborate]
   [top.kzre.homunculus.core.types.elaborate.protocol :as cfg]
   [top.kzre.homunculus.core.types.infer.core :as infer]
   [top.kzre.homunculus.core.types.loading.pass :as loading]
   [top.kzre.homunculus.core.types.mutability.core :as mut]
   [top.kzre.homunculus.core.types.recur-elim.core :as recur-elim]
   [top.kzre.homunculus.core.types.typed.core :as typed]
   [top.kzre.homunculus.internal.protocol :as p]))

(defn- default-elab-config []
  (reify cfg/IElaborateConfig
    (max-iterations [_] 5)
    (strict-mode? [_] true)
    (allow-return-closure? [_] false)
    (on-unresolved [_ lambda] (throw (ex-info "Unresolved closure" {:lambda lambda})))
    (should-inline? [_ _ _] true)))

(defrecord HLSLCompiler [frontend]
  p/ICompiler
  (compile [this forms context]
    (let [elab-config (default-elab-config)
          ir1-root   (ir1/->ir1 forms)
          ir2-roots  (ir2/lower [ir1-root])
          ;; 执行 namespace-pass 处理 ns 声明
          ir2-roots' (loading/namespace-pass ir2-roots context)
          no-recur   (mapv recur-elim/eliminate ir2-roots')
          elaborated (elaborate/elaborate no-recur elab-config)
          mutable    (mut/analyze elaborated)
          checked-fn (builtin/check mutable (merge {} (hlsl-front/builtins)))
          inferred   (infer/infer checked-fn :frontend frontend)
          typed      (typed/type-check inferred :frontend frontend :builtins (merge {} (hlsl-front/builtins)))
          checked    (check/check-program typed {:backend this})]
      (emit/generate checked this []))))
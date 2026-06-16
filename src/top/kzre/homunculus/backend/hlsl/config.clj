(ns top.kzre.homunculus.backend.hlsl.config
  "HLSL 后端的 ICompiler 实现，复用现有 Pass 管线。"
  (:require
    [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
    [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
    [top.kzre.homunculus.backend.shader.api :as shader]
    [top.kzre.homunculus.core.ir1.api :as ir1]
    [top.kzre.homunculus.core.ir2.api :as ir2]
    [top.kzre.homunculus.core.types.check.api :as check]
    [top.kzre.homunculus.core.types.constraint.api :as solve]
    [top.kzre.homunculus.core.types.lambda-elim.api :as lambda-elim]
    [top.kzre.homunculus.core.types.lambda-elim.protocol :as lambda-elim-p]
    [top.kzre.homunculus.core.types.infer.api :as infer]
    [top.kzre.homunculus.core.types.module.api :as module]
    [top.kzre.homunculus.core.types.mutability.core :as mut]
    [top.kzre.homunculus.core.types.recur-elim.api :as recur]
    [top.kzre.homunculus.internal.protocol :as p]))

(defn- default-elab-config []
  (reify lambda-elim-p/ILiftConfig
    (max-iterations [_] 5)
    (strict-mode? [_] true)
    (on-unresolved [_ lambda] (throw (ex-info "Unresolved closure" {:lambda lambda})))
    (should-inline? [_ _ _] true)))

(defrecord HLSLCompiler []
  p/ICompiler
  (emit [this forms context]
    (let [frontend (hlsl-front/->HLSLFrontend)
          backend (hlsl-backend/->HLSLBackend)
          elab-config (default-elab-config)
          ir1-roots   (mapv ir1/->ir1 forms)
          ir2-roots  (mapcat ir2/->ir2 ir1-roots)
          ;; 执行 namespace-pass 处理 ns 声明
          ir2-roots' (module/resolve-ns ir2-roots context)
          _         (module/collect-symbols ir2-roots' context) ;; 收集符号表.
          no-recur   (mapv recur/eliminate ir2-roots') ;; 递归消除
          elaborated (lambda-elim/eliminate no-recur elab-config) ;; 闭包消除
          mutable    (mut/analyze elaborated)               ;; 可变性分析. 用于生成 const 修饰
          inferred   (infer/infer mutable (infer/make-context frontend)) ;; 局部推断
          solved      (solve/process inferred  (solve/make-context frontend context)) ;; 约束求解
          checked    (check/check solved (check/make-context backend)) ;; 双向检查
          ]
      ;; 代码发射
      (shader/emit checked backend ))))
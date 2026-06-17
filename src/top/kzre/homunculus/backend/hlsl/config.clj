(ns top.kzre.homunculus.backend.hlsl.config
  "HLSL 后端的 ICompiler 实现，整合所有 Pass 并输出 HLSL 代码。"
  (:require
    [top.kzre.homunculus.backend.hlsl.api :as emit]
    [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
    [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
    [top.kzre.homunculus.core.ir1.api :as ir1]
    [top.kzre.homunculus.core.ir2.api :as ir2]
    [top.kzre.homunculus.core.types.check.api :as check]
    [top.kzre.homunculus.core.types.alpha-rename :as rename]
    [top.kzre.homunculus.core.types.constraint.api :as solve]
    [top.kzre.homunculus.core.types.infer.api :as infer]
    [top.kzre.homunculus.core.types.lambda-elim.api :as lambda-elim]
    [top.kzre.homunculus.core.types.lambda-elim.protocol :as lambda-elim-p]
    [top.kzre.homunculus.core.types.module.api :as module]
    [top.kzre.homunculus.core.types.mutability.core :as mut]
    [top.kzre.homunculus.core.types.recur-elim.api :as recur]
    [top.kzre.homunculus.internal.protocol :as p]))

;; ── 闭包消除配置 ──────────────────────
(defn- default-lift-config []
  (reify lambda-elim-p/ILiftConfig
    (max-iterations [_] 5)
    (strict-mode? [_] true)
    (on-unresolved [_ lambda _reason]
      (throw (ex-info "Unresolved closure" {:lambda lambda})))
    (lift-name-gen [_ _lambda]
      ;; 生成唯一的提升函数名
      (symbol (str "lifted_" (gensym "lambda"))))))

(defrecord HLSLCompiler []
  p/ICompiler
  (emit [_this forms context]
    (let [frontend   (hlsl-front/->HLSLFrontend)
          backend    (hlsl-backend/->HLSLBackend)
          lift-cfg   (default-lift-config)

          ;; 预处理
          processed (ir1/preprocess forms)

          ;; IR1 -> IR2
          ir1-roots  (mapv ir1/->ir1 processed)
          ir2-roots  (mapcat ir2/->ir2 ir1-roots)

          ;; ★ Alpha 重命名：确保所有变量名唯一，防止后续 Pass 变量捕获
          ir2-roots' (mapv rename/rename ir2-roots)

          ;; 命名空间处理（依赖注册、别名替换）
          ir2-roots' (module/resolve-ns ir2-roots' context)
          _          (module/collect-symbols ir2-roots' context)

          ;; 递归消除
          no-recur   (mapv recur/eliminate ir2-roots')

          ;; 闭包消除（提升 + 特化）
          elaborated (lambda-elim/eliminate no-recur lift-cfg)

          ;; 可变性分析
          mutable    (mut/analyze elaborated)

          ;; 局部类型推导
          inferred   (infer/infer mutable (infer/make-context frontend))

          ;; HM(X) 约束求解
          solved     (solve/process inferred (solve/make-context context frontend backend))

          ;; 双向检查 + 隐式转换插入
          checked    (check/check solved (check/make-context frontend backend))

          ;; 收集标记了类型的符号表
          _          (module/collect-symbols ir2-roots' context)]

      ;; 最终代码生成（HLSL）
      (emit/emit checked))))
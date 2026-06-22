(ns top.kzre.homunculus.backend.hlsl.config
  "HLSL 后端的 ICompiler 实现，整合所有 Pass 并输出 HLSL 代码。"
  (:require
    [top.kzre.homunculus.backend.hlsl.api :as emit]
    [top.kzre.homunculus.backend.hlsl.backend :as hlsl-backend]
    [top.kzre.homunculus.backend.hlsl.frontend :as hlsl-front]
    [top.kzre.homunculus.core.ir1.api :as ir1]
    [top.kzre.homunculus.core.ir2.api :as ir2]
    [top.kzre.homunculus.core.types.ho-elim.core :as ho-elim]
    [top.kzre.homunculus.core.types.check.api :as check]
    [top.kzre.homunculus.core.types.dc-elim.core :as dce]
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
    (max-iterations [_] 1000)
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

          processed (ir1/preprocess forms)
          ir1-roots  (mapv ir1/->ir1 processed)
          ir2-roots  (mapcat ir2/->ir2 ir1-roots)
          ir2-roots' (mapv rename/rename ir2-roots)

          ir2-roots' (module/resolve-ns ir2-roots' context)
          _          (module/collect-symbols ir2-roots' context)

          ;; 高阶消除
          inlined    (ho-elim/process ir2-roots' (ho-elim/make-context context frontend backend))

          ;; 死代码消除
          no-ho      (dce/eliminate-ho-defs inlined context)

          ;; 闭包消除
          no-closure (lambda-elim/eliminate no-ho lift-cfg)

          ;; ★ 递归消除提前到类型推导之前
          no-recur   (mapv recur/eliminate no-closure)

          ;; 类型推导基于已消除递归的 IR
          inferred   (infer/infer no-recur (infer/make-context context frontend backend))
          ;; 约束求解
          solved     (solve/process inferred (solve/make-context context frontend backend))

          ;; 可变性分析
          mutable    (mut/analyze solved)
          ;; 双向检查 + 隐式转换插入
          checked    (check/check mutable (check/make-context context frontend backend))
          _          (module/collect-symbols mutable context)]

      (emit/emit checked (emit/make-context context frontend)))))

(ns top.kzre.homunculus.compilers.firstorder
(:require
 [top.kzre.homunculus.core.ir1.api :as ir1]
 [top.kzre.homunculus.core.ir1.polyfill-meta :as pm]
 [top.kzre.homunculus.core.ir2.api :as ir2]
 [top.kzre.homunculus.core.ir2.node :as n]
 [top.kzre.homunculus.core.ir2.pass.alias :as alias]
 [top.kzre.homunculus.core.ir2.pass.check.api :as check]
 [top.kzre.homunculus.core.ir2.pass.constraint.api :as constraint]
 [top.kzre.homunculus.core.ir2.pass.dc-elim.core :as dce]
 [top.kzre.homunculus.core.ir2.pass.fold.core :as fold]
 [top.kzre.homunculus.core.ir2.pass.ho-elim.api :as ho-elim]
 [top.kzre.homunculus.core.ir2.pass.infer.api :as infer]
 [top.kzre.homunculus.core.ir2.pass.inline.api :as inline]
 [top.kzre.homunculus.core.ir2.pass.lambda-elim.api :as lambda-elim]
 [top.kzre.homunculus.core.ir2.pass.mark-trait :as mark-trait]
 [top.kzre.homunculus.core.ir2.pass.module.api :as module]
 [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
 [top.kzre.homunculus.core.ir2.pass.recur-elim.api :as recur-elim]
 [top.kzre.homunculus.core.ir2.pass.type :as ty]
 [top.kzre.homunculus.core.ir2.transform.rename.rename :as rename]
 [top.kzre.homunculus.internal.module-unit :as mu]
 [top.kzre.homunculus.internal.protocol :as p]))


(defn solve-fold
  "循环执行 clear → solve → fold，直到 fold 不再改变节点。
   退出时返回 solved（保留类型）。"
  [nodes ctx]
  (let [frontend (p/frontend ctx)
        backend (p/backend ctx)
        folder (tp/folder backend)
        solve (fn [n] (constraint/solve n ctx))
        fold* (fn [n] (fold/fold n (fold/make-context ctx frontend backend folder)))
        clear (fn [n] (mapv ty/clear-type n))]
    (loop [current nodes]
      (let [cleared (clear current)
            solved  (solve cleared)
            {:keys [nodes changed?]}  (fold* solved)]
        (if (not changed?)
          solved
          (recur nodes))))))

(defrecord TypedCompiler []
  p/ICompiler
  ;; 模块编译
  (compile [_ forms ctx]
    (let [frontend (p/frontend ctx)
          backend (p/backend ctx)
          ns-sym    (some-> (first forms) (nth 1))
          _         (when (nil? ns-sym)
                      (throw (ex-info "No ns form found" {:forms forms})))
          unit (mu/make-module-unit ns-sym)
          processed (ir1/preprocess forms)
          ir1-roots (mapv ir1/->ir1 processed)
          ir1-roots1 (pm/polyfill-nodes ir1-roots)
          ir2-roots (ir2/lower-nodes ir1-roots1 ctx)
          ir2-roots' (rename/rename ir2-roots)
          ir2-roots' (alias/alias-nodes ir2-roots' ctx frontend)
          ir2-roots' (module/resolve-ns ir2-roots' ctx frontend)
          unit1      (module/collect-symbols ir2-roots' ctx unit)
          traited-roots (mark-trait/mark ir2-roots')   ;; 分析标记
          ir2-roots' (inline/inline traited-roots (inline/make-env ctx))  ;; 执行内联
          no-ho      (ho-elim/elim ir2-roots' (ho-elim/make-env ctx))
          no-closure (lambda-elim/elim no-ho (lambda-elim/make-env))
          no-recur   (recur-elim/elim no-closure)
          inferred   (infer/infer no-recur (infer/make-env  ctx))
          ;solved     (solve/process inferred (solve/make-context ctx frontend backend))
          solved     (solve-fold inferred ctx)
          ;mutable    (mut/analyze solved)
          unit2      (module/collect-symbols solved ctx unit1)
          unit3      (assoc unit2 :nodes solved)]
      (p/set-module-unit! ctx unit3)
      unit3))

  (compile-module [_ unit context]
    (let [roots   (mu/module-nodes unit)
          dce-ctx (dce/make-context context)                ;; 这些是必须在HLSL 消除的代码
          no-ho (dce/eliminate-ho-defs roots dce-ctx)
          inlined (dce/eliminate-inline-defs no-ho dce-ctx)
          no-poly (dce/eliminate-polymorphic-defs inlined dce-ctx)
          emitter (p/emitter context)
          checked   (check/check no-poly (check/make-env context))
          result    (p/emit emitter checked context {:unit unit})]
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
          checked   (check/check roots (check/make-env context))
          ;; 代码生成
          result    (p/emit emitter checked context {})]
      result)))

(defonce compiler (->TypedCompiler))
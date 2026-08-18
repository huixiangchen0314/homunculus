(ns top.kzre.homunculus.core.types.constraint.core
  "约束系统的编排入口：构造上下文、运行约束生成与求解。"
  (:require
    [top.kzre.homunculus.core.types.constraint.env :as env]
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.gen.methods.array]
    [top.kzre.homunculus.core.types.constraint.gen.methods.assign]
    [top.kzre.homunculus.core.types.constraint.gen.methods.block]
    [top.kzre.homunculus.core.types.constraint.gen.methods.call]
    [top.kzre.homunculus.core.types.constraint.gen.methods.convert]
    [top.kzre.homunculus.core.types.constraint.gen.methods.define]
    [top.kzre.homunculus.core.types.constraint.gen.methods.if]
    [top.kzre.homunculus.core.types.constraint.gen.methods.lambda]
    [top.kzre.homunculus.core.types.constraint.gen.methods.let]
    [top.kzre.homunculus.core.types.constraint.gen.methods.literal]
    [top.kzre.homunculus.core.types.constraint.gen.methods.loop]
    [top.kzre.homunculus.core.types.constraint.gen.methods.map]
    [top.kzre.homunculus.core.types.constraint.gen.methods.member-access]
    [top.kzre.homunculus.core.types.constraint.gen.methods.ns]
    [top.kzre.homunculus.core.types.constraint.gen.methods.protocol]
    [top.kzre.homunculus.core.types.constraint.gen.methods.record]
    [top.kzre.homunculus.core.types.constraint.gen.methods.try]
    [top.kzre.homunculus.core.types.constraint.gen.methods.variable]
    [top.kzre.homunculus.core.types.constraint.gen.methods.vector]
    [top.kzre.homunculus.core.types.constraint.gen.methods.while]
    [top.kzre.homunculus.core.types.constraint.protocol :as p]
    [top.kzre.homunculus.core.types.constraint.unify :as u]
    [top.kzre.homunculus.core.types.protocol :as tp]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.internal.symbol :as sym]))

(defn make-context
  "构造约束生成所需的上下文 map。
   compile-ctx : 编译上下文
   frontend    : 前端协议实例（必须实现 IFrontendInfo）
   backend     : 后端协议实例（可选，用于类型转换）"
  [compile-ctx frontend backend]
  (let [builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    {:env {}
     :frontend frontend
     :ctx compile-ctx
     :backend backend
     :symbol-table symbols
     :known-types (sym/types-symbols symbols)}))



(defn solve
  [asts ctx]
  (let [{:keys [nodes constraints]}
        (gen/gen-constraints asts (make-context ctx (ip/frontend ctx) (ip/backend ctx)))]
    (loop [constrs constraints
           subs-map {}
           env (env/make-env ctx)]
      (let [[remaining subst-map' env']
            (reduce
              (fn [[cs substs e] c]
                (let [s (p/solve-constraint c substs e)
                      c' (p/substitute-constraint c s)]
                  (if (p/solved? c')
                    [cs s e]
                    [(conj cs c') s e])))
              [[] subs-map env]
              constrs)]
        (if (or (empty? remaining)
                 (= subs-map subst-map'))
          (u/subst-nodes nodes subst-map')
          (recur remaining subst-map' env'))))))
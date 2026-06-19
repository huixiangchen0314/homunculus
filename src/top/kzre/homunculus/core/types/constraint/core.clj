(ns top.kzre.homunculus.core.types.constraint.core
  "约束系统的编排入口：构造上下文、运行约束生成与求解。"
  (:require
    [top.kzre.homunculus.core.types.constraint.gen.core :as gen]
    [top.kzre.homunculus.core.types.constraint.solve :as solve]
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

(defn process
  "对 IR2 节点树进行约束生成与求解，返回类型标注后的新 IR。"
  [ir2-roots context]
  (let [{:keys [roots constraints]} (gen/generate-constraints ir2-roots context)
        conversion-fn (when-let [be (:backend context)]
                        (fn [s d] (tp/type-conversion be s d)))
        subst (solve/solve-constraints constraints conversion-fn)
        typed-roots (mapv #(solve/apply-subst % subst) roots)]
    typed-roots))
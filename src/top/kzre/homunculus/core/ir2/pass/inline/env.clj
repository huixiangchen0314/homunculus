(ns top.kzre.homunculus.core.ir2.pass.inline.env
  "内联 pass 的上下文。

   隔离外部编译上下文——pass 内部只通过协议方法访问，
   不直接接触符号表 / frontend / backend 等结构。
   这些外部依赖都从 compile-ctx 查询，不需要调用方传。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.internal.protocol :as ip]))

(defprotocol IEnv
  (find-inlineable [this fn-name]
    "返回 fn-name 对应的可内联 lambda 节点，找不到返回 nil。
     同时考虑局部内联定义和外部符号表。")
  (register-inline-def [this name lambda]
    "注册一个局部内联定义。返回新 env。"))

(defrecord Env [compile-ctx
                symbol-table
                inline-polymorphic?
                local-inline-defs]
  IEnv
  (find-inlineable [_ fn-name]
    (or (get local-inline-defs fn-name)
        (when-let [entry (get symbol-table fn-name)]
          (when (or (:inline entry)
                    (and (:polymorphic entry) inline-polymorphic?))
            (:ir2 entry)))))
  (register-inline-def [this name lambda]
    (assoc-in this [:local-inline-defs name] lambda)))

(defn make-env
  "从 compile-ctx 构造内联 pass 的环境。"
  [compile-ctx & {:keys [inline-polymorphic?] :or {inline-polymorphic? true}}]
  (let [frontend      (ip/frontend compile-ctx)
        builtin-table (tp/builtin-symbols frontend)
        user-table    (ip/symbol-table compile-ctx)
        symbols       (merge builtin-table user-table)]
    (->Env compile-ctx
           symbols
           inline-polymorphic?
           {})))
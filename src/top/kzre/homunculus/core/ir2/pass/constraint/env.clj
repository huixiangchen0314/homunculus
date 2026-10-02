(ns top.kzre.homunculus.core.ir2.pass.constraint.env
  "约束求解的环境。

   compile-ctx / backend / 符号表全部封装在内——
   约束实现只通过协议方法访问。

   - conversion-cost     查询类型转换代价
   - resolve-field-type  解析 record 字段类型"
  (:require
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.core.symbol :as sym]))

(defprotocol IEnv
  (conversion-cost [this src-type dst-type]
    "返回从 src-type 到 dst-type 的转换代价。
     不支持转换返回 nil。")
  (resolve-field-type [this record-name field-name]
    "解析 record 的字段类型。
     record 不存在或字段不存在返回 nil。"))

(defrecord Env [compile-ctx backend symbol-table]
  IEnv
  (conversion-cost [_ src-type dst-type]
    (when backend
      (tp/type-conversion backend src-type dst-type)))
  (resolve-field-type [_ record-name field-name]
    (let [field-sym (if (keyword? field-name)
                      (symbol (name field-name))
                      field-name)]
      (when-let [record-entry (sym/lookup-record symbol-table record-name)]
        (sym/lookup-field-type record-entry field-sym)))))

(defn make-env
  "从 compile-ctx 构造约束求解环境。"
  [compile-ctx]
  (let [backend (ip/backend compile-ctx)
        symbols (merge (tp/builtin-symbols (ip/frontend compile-ctx))
                       (ip/symbol-table compile-ctx))]
    (->Env compile-ctx backend symbols)))
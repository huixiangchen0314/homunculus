(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.env
  "约束生成的环境。

   compile-ctx / frontend / backend / 符号表全部封装在内——
   pass 内部只通过协议方法访问，看不到原始上下文、看不到符号表条目。

   解析入口：
   - resolve-var-type  变量名 → 类型（本地优先，全局兜底，自动实例化 scheme）
   - resolve-callees   函数名 → 候选函数类型列表
   pass 不需要知道结果来自本地还是全局。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.env :as e]
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.type :as t]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.core.symbol :as sym]))

(defprotocol IEnv
  ;; ── 解析 ──
  (resolve-var-type [this name]
    "解析变量名对应的类型。
     本地绑定优先，全局符号表兜底。
     绑定为 scheme 则自动实例化。
     找不到返回 nil。")

  (resolve-callees [this name]
    "解析函数名对应的候选函数类型列表。
     本地绑定若为函数类型则返回单元素列表；
     否则从全局符号表构造 arities。
     非函数或找不到返回 []。")

  ;; ── 作用域扩展 ──
  (bind-var [this name type]
    "在当前作用域绑定 name → type，返回新环境。")

  ;; ── 前端能力 ──
  (literal-type [this literal-value])
  (truthy-type  [this])
  (integer-type [this])

  ;; ── 后端能力 ──
  (use-hetero-vec? [this])

  ;; ── 循环状态 ──
  (loop-vars [this])
  (with-loop-vars [this vars]))

(defn- lookup-local [type-scope name]
  (or (e/lookup-env type-scope name)
      (e/lookup-env type-scope (symbol name))))

(defn- instantiate-if-scheme [binding]
  (if (scheme/tscheme? binding)
    (scheme/instantiate binding)
    binding))

(defrecord Env [type-scope symbols compile-ctx frontend backend loop-vars-list]
  IEnv
  (resolve-var-type [_ name]
    (if-let [binding (lookup-local type-scope name)]
      (instantiate-if-scheme binding)
      (when-let [entry (sym/lookup-in-tables name symbols)]
        (when-let [type-entry (sym/entry->type entry)]
          (:type type-entry)))))

  (resolve-callees [_ name]
    (if-let [binding (lookup-local type-scope name)]
      (let [type (instantiate-if-scheme binding)]
        (if (t/fun-type? type) [type] []))
      (when-let [entry (sym/lookup-in-tables name symbols)]
        (when-let [function-entry (sym/entry->func entry)]
          (mapv t/arity->tfun (sym/list-arities function-entry))))))

  (bind-var [this name type]
    (assoc this :type-scope (e/extend-env type-scope name type)))

  (literal-type [_ literal-value]
    (when frontend (tp/literal->type frontend literal-value)))
  (truthy-type [_]
    (when frontend (tp/truly-type frontend)))
  (integer-type [_]
    (tp/integer-type frontend))

  (use-hetero-vec? [_]
    (when backend (tp/support-hetero-vec backend)))

  (loop-vars [_] loop-vars-list)
  (with-loop-vars [this vars]
    (assoc this :loop-vars-list vars)))

(defn make-env [compile-ctx]
  (let [frontend (ip/frontend compile-ctx)
        backend  (ip/backend compile-ctx)
        symbols  (merge (tp/builtin-symbols frontend)
                        (ip/symbol-table compile-ctx))]
    (->Env {} symbols compile-ctx frontend backend nil)))
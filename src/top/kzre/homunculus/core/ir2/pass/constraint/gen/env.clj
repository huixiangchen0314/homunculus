(ns top.kzre.homunculus.core.ir2.pass.constraint.gen.env
  "约束生成的环境。

   compile-ctx / frontend / backend / 符号表全部封装在内——
   pass 内部只通过协议方法访问，看不到原始上下文、看不到符号表条目。

   解析入口：
   - resolve-var-type  变量名 → 类型（本地优先，全局兜底，自动实例化 scheme）
   - resolve-callees   函数名 → 候选函数类型列表
   pass 不需要知道结果来自本地还是全局。"
  (:require
    [top.kzre.homunculus.core.ir2.pass.protocol :as tp]
    [top.kzre.homunculus.core.ir2.pass.constraint.scheme :as scheme]
    [top.kzre.homunculus.core.ir2.pass.type :as t]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.core.symbol :as sym]))

;; ═══════════════════════════════════════════════
;; 协议
;; ═══════════════════════════════════════════════

(defprotocol IEnv
  ;; ── 类型解析 ──
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
  (unbind-var [this name]
    "移除 name 在当前作用域的绑定，返回新环境。
     名字不存在时返回等价环境（幂等）。")

  (generalize [this type]
    "对 type 做泛化。
     具体类型返回原类型；
     函数类型按当前作用域泛化；
     其它类型原样返回。")

  ;; ── 前端能力 ──
  (literal-type [this literal-value]
    "字面量值对应的类型，未知返回 nil。")
  (truthy-type [this]
    "真值上下文（if / while）所需的类型，不需要则返回 nil。")
  (integer-type [this]
    "整数类型。")

  ;; ── 后端能力 ──
  (use-hetero-vec? [this]
    "后端是否支持异构向量。")
  (conversion-cost [this src-type dst-type]
    "返回从 src-type 到 dst-type 的转换代价。
     不支持转换返回 nil。")

  ;; ── 循环状态 ──
  (loop-vars [this]
    "当前所在循环的变量名列表，非循环中返回 nil。")
  (with-loop-vars [this vars]
    "设置当前循环变量，返回新环境。"))

;; ═══════════════════════════════════════════════
;; 内部辅助
;; ═══════════════════════════════════════════════

(defn- lookup-local
  "在局部类型作用域中查找 name，兼容 symbol / keyword 两种键。"
  [type-scope name]
  (or (get type-scope name)
      (get type-scope (symbol name))))

(defn- instantiate-if-scheme
  "若绑定为 scheme 则实例化，否则原样返回。"
  [binding]
  (if (scheme/tscheme? binding)
    (scheme/instantiate binding)
    binding))

;; ═══════════════════════════════════════════════
;; Record
;; ═══════════════════════════════════════════════

(defrecord Env [type-scope
                symbols
                compile-ctx
                frontend
                backend
                loop-vars-list]
  IEnv
  ;; ── 类型解析 ──
  (resolve-var-type [_ name]
    (if-let [binding (lookup-local type-scope name)]
      (instantiate-if-scheme binding)
      (sym/resolve-sym-type symbols name)))

  (resolve-callees [_ name]
    (if-let [binding (lookup-local type-scope name)]
      (let [type (instantiate-if-scheme binding)]
        (if (t/fun-type? type) [type] []))
      (when-let [entry (sym/lookup-in-tables name symbols)]
        (when-let [function-entry (sym/entry->func entry)]
          (mapv t/arity->tfun (sym/list-arities function-entry))))))

  ;; ── 作用域扩展 ──
  (bind-var [this name type]
    ;; TODO 当前只支持单类型绑定。函数重载需要表达「一个名字对应多个类型」，
    ;;      未来通过引入联合类型（union type）解决——比显式重载列表更通用，
    ;;      也适用于非函数场景（如 let 绑定的值可能是多种类型）。
    (assoc this :type-scope (assoc type-scope name type)))
  (unbind-var [this name]
    (assoc this :type-scope
                (dissoc type-scope name (symbol name))))
  (generalize [_ type]
    (cond
      (t/concrete? type) type
      (t/fun-type? type) (scheme/generalize type type-scope)
      :else              type))

  ;; ── 前端能力 ──
  (literal-type [_ literal-value]
    (when frontend (tp/literal->type frontend literal-value)))
  (truthy-type [_]
    (when frontend (tp/truly-type frontend)))
  (integer-type [_]
    (when frontend (tp/integer-type frontend)))

  ;; ── 后端能力 ──
  (use-hetero-vec? [_]
    (when backend (tp/support-hetero-vec backend)))
  (conversion-cost [_ src-type dst-type]
    (when backend
      (tp/type-conversion backend src-type dst-type)))
  ;; ── 循环状态 ──
  (loop-vars [_] loop-vars-list)
  (with-loop-vars [this vars]
    (assoc this :loop-vars-list vars)))

;; ═══════════════════════════════════════════════
;; 构造
;; ═══════════════════════════════════════════════

(defn make-env
  "从 compile-ctx 构造约束生成环境。
   frontend / backend 由 compile-ctx 查询，调用方只提供上下文。"
  [compile-ctx]
  (let [frontend (ip/frontend compile-ctx)
        backend  (ip/backend compile-ctx)
        symbols  (merge (tp/builtin-symbols frontend)
                        (ip/symbol-table compile-ctx))]
    (->Env {}
           symbols
           compile-ctx
           frontend
           backend
           nil)))
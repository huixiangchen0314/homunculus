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
    [top.kzre.homunculus.core.ir2.pass.type :as ty]
    [top.kzre.homunculus.internal.protocol :as ip]
    [top.kzre.homunculus.core.symbol :as sym]
    [top.kzre.homunculus.internal.utils :as iu]))

;; ═══════════════════════════════════════════════
;; 协议
;; ═══════════════════════════════════════════════

(defprotocol IEnv
  ;; TODO 调用点驱动应当是逐实例的配置，应当用单态化进行解决，并补充旧定义的清理
  (^:deprecated call-driven-inference? [this]
    "是否允许从调用点反向约束函数类型（call-driven inference）。

     严格 Hindley-Milner 下，函数类型只在定义处确定，调用点只能
     实例化已有 scheme，不能反向传播类型。静态语言要支持闭包时，
     闭包捕获的自由变量类型往往不在定义处完整可知，必须允许调用点
     反向约束——本开关控制这一行为。

     开启：调用点的实参类型参与约束被调函数的 TVar。
     关闭：被调函数的 TVar 只能在定义处被约束。

     命名由来：与 let-generalization（定义驱动泛化）对偶——
     let 从定义处泛化，call-driven 从调用点具体化。")

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
  (resolve-field-type [this record-name field-name]
    "解析 record 的字段类型。
     record 不存在或字段不存在返回 nil。
     注意字段和协议方法是被显示区分的，这个仅负责获取字段")
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

;; ═══════════════════════════════════════════════
;; Record
;; ═══════════════════════════════════════════════

(defrecord Env [type-scope
                compile-ctx
                loop-vars-list]
  IEnv
  (call-driven-inference? [_]
    (-> compile-ctx
        (ip/config)
        ;; 调用驱动接口，当使能这个时候
        ;; 对于标记 :called-driven-interface?的多态函数，将从调用点约束其类型
        (ip/get-option :called-driven-interface?)))
  ;; ── 类型解析 ──
  (resolve-var-type [_ name]
    (if-let [binding (lookup-local type-scope name)]
      (scheme/instantiate-if-scheme binding)
      (when-let [entry (ip/resolve-symbol-entry compile-ctx name)]
        (sym/entry->symbol-type entry))))
  (resolve-field-type [_ record-name field-name]
    (when-let [entry  (ip/resolve-symbol-entry compile-ctx record-name)]
      (when-let [record-entry (sym/entry->record entry)]
        (sym/lookup-field-type record-entry (iu/ensure-symbol field-name)))))

  (resolve-callees [_ name]
    (if-let [binding (lookup-local type-scope name)]
      (let [type (scheme/instantiate-if-scheme binding)]
        (if (ty/fun-type? type) [type] []))
      (when-let [entry (ip/resolve-symbol-entry compile-ctx name)]
        (when-let [function-entry (sym/entry->func entry)]
          (mapv ty/arity->tfun (sym/list-arities function-entry))))))

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
      (ty/concrete? type) type
      (ty/fun-type? type) (scheme/generalize type type-scope)
      :else              type))

  ;; ── 前端能力 ──
  (literal-type [_ literal-value]
    (-> compile-ctx
        (ip/frontend)
        (tp/literal->type literal-value)))
  (truthy-type [_]
    (-> compile-ctx
        (ip/frontend)
        (tp/truly-type)))
  (integer-type [_]
    (-> compile-ctx
        (ip/frontend)
        (tp/integer-type)))
  ;; ── 后端能力 ──
  (use-hetero-vec? [_]
    (-> compile-ctx
        (ip/backend)
        (tp/support-hetero-vec)))
  (conversion-cost [_ src-type dst-type]
    (-> compile-ctx
        (ip/backend)
        (tp/type-conversion src-type dst-type)))
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
  (->Env {}
         compile-ctx
         nil))
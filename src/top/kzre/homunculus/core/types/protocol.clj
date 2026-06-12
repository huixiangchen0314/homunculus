;; ═══════════════════════════════════════════════════════
;; top.kzre.homunculus.core.types.protocol
;; 所有编译器类型、形状、前后端接口的协议定义
;; ═══════════════════════════════════════════════════════
(ns top.kzre.homunculus.core.types.protocol)

;; ── 类型表示协议 ──────────────────────────
(defprotocol IType
  (type-kind [this] "返回 :var, :con, :fun, :app, :container 等"))

;; ── 集合形状协议 ──────────────────────────
(defprotocol ICollectionShape
  (shape-kind [this] "返回 :fixed, :variable, :map, :set 等"))

;; ── 前端语言信息 ──────────────────────────
(defprotocol IFrontendInfo
  (frontend-types      [this] "返回源语言支持的所有类型构造器（关键字向量）")
  (literal->type       [this value] "根据 Clojure 字面量推断前端类型（IType）")
  (meta->type          [this meta-node] "从元数据提取类型标注")
  (infer-collection-type [this form] "根据 Clojure 集合字面量推断 TContainer")
  (collection-type-ctor [this kind element-type shape] "构造一个 TContainer")
  (builtin-functions [this]
    "返回该语言的内置函数类型环境，键为符号，值为 IType（通常为 TFun 或多态类型）。HM 推断以此为基础。")
  )

;; ── 后端信息 ──────────────────────────────
(defprotocol IBackendInfo
  (prims               [this] "返回原语列表（每个实现 IFuncInfo）")
  (builtin-type?       [this ty-name] "判断是否为后端内置类型")
  (strictness          [this] "严格性配置 map")
  (type-conversion     [this src-ty dst-ty] "两个内置类型间的转换代价")
  (resolve-container   [this container-ty] "将前端 TContainer 转为目标语言类型")
  (backend-container-type [this kind element-ty shape] "根据种类/元素/形状生成目标类型"))

;; ── 函数信息 ──────────────────────────────
(defprotocol IFuncInfo
  (func-name      [this] "函数名（符号）")
  (func-arity     [this] "参数个数或 :variadic")
  (param-types    [this] "参数类型列表（IType 向量）")
  (return-type    [this] "返回类型（IType）")
  (pure?          [this] "是否为纯函数")
  (side-effects   [this] "副作用描述（关键字或 nil）")
  (variadic?      [this] "是否可变参数"))

(defprotocol IInlineLiftConfig
  "内联/提升的配置协议。"
  (should-inline? [this lambda-node call-site]
    "根据 lambda 节点和调用点决定是否内联。返回 true/false。")
  (should-lift? [this lambda-node]
    "根据 lambda 节点决定是否提升到顶层。返回 true/false。")
  (max-inline-size [this]
    "返回允许内联的最大体大小（节点数）。")
  (lift-name-gen [this lambda-node]
    "为提升的函数生成顶层名称。"))
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
  (literal->type       [this value] "根据 Clojure 字面量推断前端类型（IType）. 这个必须保留，因为 Clojure 字面量在其他语言可能是类型.无法简单从符号表推断")
  (meta->type          [this meta-node] "从元数据提取类型标注")
  (builtin-functions [this]
    "返回该语言的内置函数类型环境，键为符号，值为 IType（通常为 TFun 或多态类型）。HM 推断以此为基础。")
  (builtin-symbols   [this]
    "返回内置符号表，符合 ::spec/symbol-table 规范。
     包含所有内置类型、函数、记录、协议、变量的类型信息。")
  (truly-type [this] "返回语言的真值类型，nil 表示无特定真值类型，可能有特殊的真值规则")
  (dynamic? [this] "语言是否是动态类型的.")
  (macro-namespaces [this]
    "返回一个集合（符号），表示仅用于编译时宏展开的命名空间，
     这些依赖不应生成 #include 指令。"))

;; ── 后端信息 ──────────────────────────────
(defprotocol IBackendInfo
  (type-conversion     [this src-ty dst-ty] "两个内置类型间的转换代价"))

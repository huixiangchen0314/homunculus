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
     包含所有内置类型、函数、记录、协议、变量的类型信息。"))

;; ── 后端信息 ──────────────────────────────
(defprotocol IBackendInfo
  (type-conversion     [this src-ty dst-ty] "两个内置类型间的转换代价"))

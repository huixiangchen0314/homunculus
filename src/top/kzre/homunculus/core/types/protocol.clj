(ns top.kzre.homunculus.core.types.protocol)

;; ── 类型表示协议 ──────────────────────────
(defprotocol IType
  (type-kind [this] "返回 :var, :con, :fun, :app, :container 等"))


;; ── 前端语言信息 ──────────────────────────
(defprotocol IFrontendInfo
  (literal->type       [this value] "根据 Clojure 字面量推断前端类型（IType）. 这个必须保留，因为 Clojure 字面量在其他语言可能是类型.无法简单从符号表推断")
  (builtin-symbols   [this]
    "返回内置符号表，符合 ::spec/symbol-table 规范。
     包含所有内置类型、函数、记录、协议、变量的类型信息。")
  (truly-type [this] "返回语言的真值类型，nil 表示无特定真值类型，可能有特殊的真值规则")
  (integer-type [this] "返回语言的整数类型.")
  (macro-namespaces [this]
    "返回一个集合（符号），表示仅用于编译时宏展开的命名空间，
     这些依赖不应生成 #include 指令。")
  )

;; ── 后端信息 ──────────────────────────────
(defprotocol IBackendInfo
  (type-conversion     [this src-ty dst-ty] "两个内置类型间的转换代价")
  (support-hetero-vec [this] "支持异构向量吗")
  (folder [this] "返回常量折叠器函数，接收一个表达式节点，返回折叠后的节点或原节点"))

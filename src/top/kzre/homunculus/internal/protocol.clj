(ns top.kzre.homunculus.internal.protocol
  "编译器内部协议：配置、上下文与编译器入口。")

(defprotocol ICompileConfig
  "一次编译指令所需的静态配置。"
  (source-paths [this] "入口源文件路径列表")
  (lib-paths    [this] "库文件的搜索路径列表")
  (output-dir   [this] "编译输出目录")
  (module-naming-style [this]
    "模块命名风格：:default、:flat 或 :flat-snake。
     :default - Java 风格，点分隔目录，例如 a.b.c -> a/b/c.hlsl
     :flat    - 所有输出在同一目录，include 路径为 a.b.c.hlsl
     :flat-snake - 类似 flat，但将点替换为下划线，例如 a_b_c.hlsl"))


(defprotocol ICompileContext
  (config           [this])
  (register-deps    [this dep-syms] "递归编译所有依赖，确保它们已就绪")
  (register-sym [this sym-entry] "注册一个符号表项")
  (symbol-table      [this ] "返回全局符号表"))


(defprotocol ICompiler
  "编译器后端。支持模块化编译与链接。"
  (compile-module [this ns-sym context]
    "编译单个命名空间模块，返回 ModuleUnit。
     会递归编译依赖，执行 IR 构建、类型推断及约束求解，
     但不执行全局死代码消除、最终类型检查及代码生成。")
  (link [this context]
    "收集所有已编译的 ModuleUnit，执行全局优化和检查，
     最终生成目标代码字符串。")
  ;; 保留单文件便捷方法
  (emit [this forms context]
    "单文件全流程编译，通常用于测试。"))

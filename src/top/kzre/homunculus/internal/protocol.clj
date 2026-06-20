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
  "完整的编译器后端：接收合并后的表单序列，产出目标代码。"
  (emit [this forms context] "编译表单序列，返回目标代码字符串。"))

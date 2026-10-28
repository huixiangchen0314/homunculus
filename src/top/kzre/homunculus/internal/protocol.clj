(ns top.kzre.homunculus.internal.protocol
  "编译器内部协议：配置、上下文与编译器入口。"
  (:refer-clojure :exclude [compile]))

(defprotocol ICompileConfig
  "一次编译指令所需的静态配置。"
  (options [this] "原始 options map")
  (get-option [_ path] "返回配置选项值")
  (lib-paths    [this] "库文件的搜索路径列表")
  (output-path   [this] "编译输出目录")
  (target       [this] "目标平台，如 :hlsl :glsl")
  (module-naming-style [this]
    "模块命名风格：:default、:flat 或 :flat-snake。
     :default - Java 风格，点分隔目录，例如 a.b.c -> a/b/c.hlsl
     :flat    - 所有输出在同一目录，include 路径为 a.b.c.hlsl
     :flat-snake - 类似 flat，但将点替换为下划线，例如 a_b_c.hlsl"))


(defprotocol ICompileContext
  (config  [this] "获取配置选项 map")
  (frontend [this] "前端协议")
  (backend [this] "后端协议")
  (compiler [this] "编译器")
  (emitter [this] "代码发射器")
  (register-deps [this dep-syms] "递归编译所有依赖，确保它们已就绪")
  (register-sym [this sym-entry] "注册一个符号表项")
  (^:deprecated symbol-table [this] "返回全局符号表")
  (module-unit [this ns-sym])
  (set-module-unit! [this unit])
  (all-module-units [this] "返回积累的所有模块单元")
  (macro-namespaces [_]
    "所有宏命名空间——前端内置的 + 用户通过 :macro-ns 选项
     额外指定的。前端协议只提供内置集合，用户扩展在这一层组合。")
  (load-macro-ns! [this ns-sym]
    "加载宏命名空间到 JVM。幂等——已加载的直接返回。
     找不到源文件或加载失败时抛异常。
     加载状态由 context 持有，不在 resolver 侧维护全局状态。")
  (macro-ns-loaded? [this ns-sym]
    "宏命名空间是否已加载。")
  (ns-exported-syms [this ns-sym]
    "返回 ns-sym 对应模块的公开导出符号集合（set of symbol）。
     若尚未编译，先递归编译（ensure-compiled，幂等）。
     ns 找不到或无 module-unit 时抛 :ns-not-found。")
  (resolve-symbol-entry [this sym]
    "解析全限定符号 sym 的符号表条目。
     先查用户定义的符号表，未命中再查 builtin 符号表。
     未命中返回 nil。")
  (builtin-instruction? [this sym]))

(defprotocol IEmitter
  (emit [this nodes context opts] "发射AST"))


(defprotocol ICompiler
  "编译器后端。支持模块化编译与链接。"
  (compile [this forms context]
    "编译单个命名空间模块，返回 ModuleUnit。
     会递归编译依赖，执行 IR 构建、类型推断及约束求解，
     但不执行全局死代码消除、最终类型检查及代码生成。")
  (link [this context]
    "收集所有已编译的 ModuleUnit，执行全局优化和检查，
     返回生成代码.")
  (compile-module [this unit context]
    "编译单个 ModuleUnit，执行最终类型检查，返回生成代码.")
  )


(defprotocol ICompileResult
  (paths [this] "编译产生的文件路径")
  (content [this f] "根据编译文件路径获取该文件的内容"))

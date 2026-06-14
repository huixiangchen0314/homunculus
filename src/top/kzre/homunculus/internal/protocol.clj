(ns top.kzre.homunculus.internal.protocol
  "编译器内部协议：配置、上下文与编译器入口。")

(defprotocol ICompileConfig
  "一次编译指令所需的静态配置。"
  (source-paths [this] "入口源文件所在的根目录列表")
  (lib-paths    [this] "库文件的搜索路径列表")
  (output-dir   [this] "编译输出目录"))

(defprotocol ICompileContext
  (config           [this])
  (resolve-ns       [this ns-sym]   "加载源文件返回 forms，若已缓存则返回 nil")
  (register-deps    [this dep-syms] "递归编译所有依赖，确保它们已就绪")
  (lookup-type      [this full-name] "根据完全限定名查找类型")
  (get-export-syms  [this ns-sym]   "获取某个命名空间的导出符号表"))

(defprotocol ICompiler
  "完整的编译器后端：接收合并后的表单序列，产出目标代码。"
  (compile [this forms context] "编译表单序列，返回目标代码字符串。"))

(defprotocol IFileResolver
  "文件系统访问抽象。"
  (resolve-file [this ns-sym] "将命名空间符号转为 java.io.File，若找不到返回 nil")
  (read-content [this file]   "读取文件，返回其文本内容字符串"))
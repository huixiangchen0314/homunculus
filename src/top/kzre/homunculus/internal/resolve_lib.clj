(ns top.kzre.homunculus.internal.resolve-lib
  "从 lib-paths（目录或 jar）解析并读取命名空间源文件。
   lib-paths 元素可为目录或 .jar；目录按 .clj 文件读取，jar 按 entry 读取。
   全部未命中时回退到 JVM classpath 资源。"
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [top.kzre.homunculus.internal.utils :as util])
  (:import
    (java.io FileNotFoundException IOException)
    (java.util.jar JarFile)
    (java.util.zip ZipException)))

;; ── 命名空间 → 路径 ─────────────────────

(defn ns->path
  "命名空间符号 → 相对于 base-path 的源文件路径（.clj）。
   base-path 末尾的 / 或 \\ 会被去掉。"
  [base-path ns-sym]
  (let [relative (-> (name ns-sym)
                     (str/replace "." "/")
                     (str/replace "-" "_")
                     (util/ensure-suffix ".clj"))]
    (str (str/replace (str base-path) #"[/\\]+$" "") "/" relative)))

(defn- ns->classpath
  "命名空间符号 → jar entry / classpath 资源路径（.clj）。"
  [ns-sym]
  (-> (name ns-sym)
      (str/replace "." "/")
      (str/replace "-" "_")
      (util/ensure-suffix ".clj")))

(defn module-candidates
  "命名空间 ns-sym 在 lib-paths 下所有目录候选路径（有序）。"
  [lib-paths ns-sym]
  (let [paths (if (coll? lib-paths) lib-paths [lib-paths])]
    (mapv #(ns->path % ns-sym) paths)))

;; ── 文件 / jar 读取 ─────────────────────

(defn- read-file
  [path]
  (try (slurp path)
       (catch FileNotFoundException _ nil)
       (catch IOException _ nil)))

(defn- jar-file?
  "路径是否以 .jar 结尾（大小写不敏感）。"
  [path]
  (boolean (re-find #"(?i)\.jar$" (str path))))

(defn- read-jar-entry
  "从 jar 读取 entry 内容。entry 不存在、jar 损坏或 IO 失败均返回 nil。"
  [jar-path entry]
  (try
    (with-open [jf (JarFile. (io/file (str jar-path)))]
      (when-let [e (.getJarEntry jf entry)]
        (with-open [in (.getInputStream jf e)]
          (slurp in))))
    (catch FileNotFoundException _ nil)
    (catch ZipException _ nil)
    (catch IOException _ nil)))

(def ^:private read-jar-entry*
  "memoize 包装：同一进程内同一 (jar, entry) 只读一次。nil 也缓存。"
  (memoize read-jar-entry))

;; ── 对外 ────────────────────────────────

(defn resolve-file
  "从候选路径中读取第一个可读文件的内容，找不到返回 nil。"
  [paths]
  (some read-file paths))

(defn- read-from-lib-path
  "从单个 lib-path（目录或 jar）读取 ns 源内容，未命中返回 nil。
   目录 → 拼 .clj 文件路径；jar → 查 jar entry。"
  [lib-path ns-sym]
  (if (jar-file? lib-path)
    (read-jar-entry* lib-path (ns->classpath ns-sym))
    (read-file (ns->path (str lib-path) ns-sym))))

(defn read-ns-source
  "从 lib-paths 查找模块源文件，返回源字符串或 nil。

   lib-paths 元素是目录 → 拼 .clj 文件路径读取
   lib-paths 元素是 .jar → 从 jar entry 读取
   都未命中 → 回退 JVM classpath（用于 cljh 自身打包的资源/标准库）"
  [lib-paths ns-sym]
  (let [paths (if (coll? lib-paths) lib-paths [lib-paths])
        entry (ns->classpath ns-sym)]
    (or (some #(read-from-lib-path % ns-sym) paths)
        (when-let [res (io/resource entry)]
          (slurp res)))))
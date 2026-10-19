(ns top.kzre.homunculus.cli.load
  "按 macro-ns 列表在 -L 路径下逐个加载用户宏命名空间。

   不走 classpath 扫描——只加载明确指定的 ns。每个 ns 转成文件
   路径后在 -L 路径下逐个匹配，找到即 load-file。"
  (:require
   [clojure.java.io :as io]
   [top.kzre.homunculus.internal.utils :as u]))

(defn- list-ns-files [lib-paths ns-sym]
  (mapv #(.getPath (io/file  (u/ns->path % ns-sym)))
        lib-paths))

(defn- find-ns-file
  "在 lib-paths 下查找 ns-sym 对应的源文件。返回 File 或 nil。"
  [lib-paths ns-sym]
  (some (fn [lib-path]
          (let [f (io/file (u/ns->path lib-path ns-sym) )]
            (when (.isFile f) f)))
        lib-paths))

(defn load-macro-namespaces!
  "按顺序加载用户宏命名空间。

   ns-list   —— 符号列表，如 ['composite.comp-one 'composite.tile-layout]
   lib-paths —— -L 路径列表

   对每个 ns，在 lib-paths 下查找源文件，找到后 load-file。
   任一 ns 找不到源文件或加载失败，抛异常。

   ── 依赖顺序 ─────────────────────────────────────────
   load-file 内部的 :require 仍走 JVM classpath——用户宏 ns 之间
   如果互相 require，那些被依赖的 ns 也要在 ns-list 里，且按依赖
   顺序在前。本函数按 ns-list 顺序逐个加载，不自动递归处理用户
   ns 之间的依赖。

   例：comp-one 需要 tile-layout 的资源声明才能在宏展开后引用——
   列表写成 ['composite.tile-layout 'composite.comp-one]，先加载
   tile-layout 再加载 comp-one。

   编译器自身的 ns（top.kzre.*）在编译器 classpath 里，无需列出。"
  [ns-list lib-paths]
  (doseq [ns-sym ns-list]
    (if-let [f (find-ns-file lib-paths ns-sym)]
      (try
        (load-file (.getAbsolutePath f))
        (catch Exception e
          (throw (ex-info (str "加载宏命名空间失败: " ns-sym
                               " (" (.getPath f) ")")
                          {:ns ns-sym :file (.getPath f)}
                          e))))
      (throw (ex-info (str "宏命名空间 " ns-sym " 在 -L 路径下找不到源文件")
                      {:ns ns-sym
                       :searched (list-ns-files lib-paths ns-sym)})))))
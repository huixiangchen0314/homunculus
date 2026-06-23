(ns top.kzre.homunculus.cli.core
  "Homunculus 编译器命令行入口。"
  (:gen-class)
  (:require [top.kzre.homunculus.backend.hlsl.config :as hlsl]
            [top.kzre.homunculus.internal.model :as model]
            [top.kzre.homunculus.internal.protocol :as p]
            [top.kzre.homunculus.internal.utils :as u]
            [top.kzre.homunculus.cli.options :as opts]))

(defn -main
  [& args]
  (let [parsed (opts/parse-opts args)
        {:keys [options files errors]} parsed]
    (when (seq errors)
      (doseq [e errors] (println "[ERROR]" e))
      (System/exit 1))
    (when (:help options)
      (println (opts/usage-string))
      (System/exit 0))
    (when (:version options)
      (println "Homunculus Compiler v0.1.0")
      (System/exit 0))
    (when (empty? files)
      (println "错误: 未指定输入文件")
      (println (opts/usage-string))
      (System/exit 1))

    (let [compiler   (hlsl/->HLSLCompiler)
          lib-paths  (if-let [libs (not-empty (:lib options))]
                       libs
                       ["."])   ;; 默认当前目录为库搜索路径
          config     (model/->CompileConfig (or (:include options) [])
                                            lib-paths
                                            (:output options))
          state      (atom {:compiling #{} :modules {} :symbol-table {}})
          context    (model/->DefaultCompileContext config compiler state)]
      ;; 逐个文件编译模块
      (doseq [file-path files]
        (let [src    (slurp file-path)
              forms  (u/parse-forms src)
              ns-sym (second (first forms))]   ; 从 (ns ...) 提取命名空间符号
          (when-not ns-sym
            (throw (ex-info "无法确定命名空间" {:file file-path})))
          (p/compile-module compiler ns-sym context)))
      ;; 链接并输出
      (let [code (p/link compiler context)]
        (println code)))))
(ns top.kzre.homunculus.cli.core
  (:gen-class)
  (:require
    [top.kzre.homunculus.backend.hlsl.config :as hlsl]
    [top.kzre.homunculus.internal.model :as model]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.utils :as u]
    [top.kzre.homunculus.cli.options :as opts]
    [clojure.java.io :as io]))

(defn- compiler-for-target [target]
  (case target
    :hlsl (hlsl/->HLSLCompiler)
    ;; 未来可添加其他后端
    (throw (ex-info (str "Unsupported target: " target) {:target target}))))

(defn -main [& args]
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

    (let [target   (keyword (get options :target "hlsl"))
          compiler (compiler-for-target target)
          lib-paths (or (not-empty (:lib options)) ["."])
          config   (model/->CompileConfig options
                                          (or (:include options) [])
                                          lib-paths
                                          (:output options)
                                          target
                                          (:style options))
          state    (atom {:compiling #{} :modules {} :symbol-table {}})
          context  (model/->DefaultCompileContext config compiler state)]

      ;; 编译所有输入文件
      (doseq [file-path files]
        (let [src   (slurp file-path)
              forms (u/parse-forms src)]
          (p/compile-module compiler forms context )))

      ;; 输出
      (if (:split-modules options)
        ;; 分割输出：每个模块单独生成文件，使用配置的命名风格
        (let [all-units (vals (get-in @state [:modules]))
              out-dir   (:output options "out")
              style     (p/module-naming-style config)]   ;; 从配置获取风格
          (doseq [unit all-units]
            (let [code     (p/emit compiler unit context)
                  ns-sym   (:ns-sym unit)
                  filename (u/ns->module-path ns-sym style ".hlsl")
                  f (io/file out-dir filename)]  ;; 根据风格生成文件名
              (io/make-parents f)
              (spit f code)
              (println "生成文件:" (str out-dir "/" filename)))))
        ;; 合并输出（单文件链接）
        (let [code (p/link compiler context)]
          (println code))))))
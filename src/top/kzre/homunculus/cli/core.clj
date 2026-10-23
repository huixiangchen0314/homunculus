(ns top.kzre.homunculus.cli.core
  (:refer-clojure :exclude [compile])
  (:gen-class)
  (:require
   [top.kzre.homunculus.backend.glsl.core :as glsl]
   [top.kzre.homunculus.backend.hlsl.core :as hlsl]
   [top.kzre.homunculus.cli.load :as load]
   [top.kzre.homunculus.cli.option :as opt]
   [top.kzre.homunculus.cli.options]
   [top.kzre.homunculus.internal.compile-result :as compile-result]
   [top.kzre.homunculus.internal.config-impl :as config-impl]
   [top.kzre.homunculus.internal.context-impl :as model]
   [top.kzre.homunculus.internal.module-unit]
   [top.kzre.homunculus.internal.protocol :as proto]
   [top.kzre.homunculus.internal.utils :as u]
   [top.kzre.homunculus.version :as version]))

(defn- lookup-compile-target [target]
  (case target
    :hlsl (hlsl/target)
    :glsl (glsl/target)
    (throw (ex-info (str "Unsupported target: " target) {:target target}))))

(defn- load-config-files
  "读取命令行 -c 指定的所有配置文件。
   :config 可能是单值或 vector；失败时打印诊断并退出。"
  [config-files]
  (if (seq config-files)
    (try
      (config-impl/load-configs config-files)
      (catch Exception e
        (println "[ERROR]" (.getMessage e))
        (when-let [data (ex-data e)]
          (when-let [p (:path data)]   (println "  配置文件:" p))
          (when-let [r (:reason data)] (println "  原因:" (name r))))
        (System/exit 1)))
    []))

(defn- write-result [result]
  (if (sequential? result)
    (doseq [res result]
      (compile-result/write-result res))
    (if (some? result)
      (compile-result/write-result result)
      (println "no module result emitted"))))

(defn compile
  "程序编译入口"
  [args]
  (let [parsed (opt/parse-opts args)
        {:keys [options files errors]} parsed]
    (when (seq errors)
      (doseq [e errors] (println "[ERROR]" e))
      (System/exit 1))
    (when (:help options)
      (println (opt/usage-string))
      (System/exit 0))
    (when (:version options)
      (println (str "Homunculus Compiler v" version/version))
      (System/exit 0))
    (when (empty? files)
      (println "错误: 未指定输入文件")
      (println (opt/usage-string))
      (System/exit 1))
    (let [configs (load-config-files (:config options))
          cfg (apply config-impl/make-compile-config options configs)]
      ;; 加载用户宏命名空间——按列表顺序，在 -L 路径下逐个查找
      (when (seq (proto/get-option cfg :macro-ns))
        (try
          (load/load-macro-namespaces! (proto/get-option cfg :macro-ns)
                                       (proto/lib-paths cfg))
          (catch Exception e
            (println "[ERROR]" (.getMessage e))
            (when-let [data (ex-data e)]
              (when (:searched data)
                (println "  已搜索路径:")
                (doseq [p (:searched data)] (println "   " p))))
            (System/exit 1))))

      (let [target (lookup-compile-target (proto/target cfg))
            context  (model/make-compile-context cfg target)
            compiler (proto/compiler context)]

        ;; 编译所有输入文件
        (doseq [file-path files]
          (let [src   (slurp file-path)
                forms (u/parse-forms src)]
            (proto/compile compiler forms context )))

        ;; 输出
        (if (proto/get-option cfg :split-modules)
          ;; 分割输出：每个模块单独生成文件，使用配置的命名风格
          (let [all-units (proto/all-module-units context)]   ;; 从配置获取风格
            (doseq [unit all-units]
              (when-let [result (proto/compile-module compiler unit context)]
                (write-result result))))
          ;; 合并输出（单文件链接）
          (let [result (proto/link compiler context)]
            (write-result result)))))))

(defn -main [& args]
  (compile args))
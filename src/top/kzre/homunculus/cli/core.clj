(ns top.kzre.homunculus.cli.core
  (:gen-class)
  (:require
    [clojure.java.io :as io]
    [top.kzre.homunculus.backend.hlsl.core :as hlsl]
    [top.kzre.homunculus.cli.options :as opts]
    [top.kzre.homunculus.internal.model :as model]
    [top.kzre.homunculus.internal.module-unit :as mu]
    [top.kzre.homunculus.internal.protocol :as p]
    [top.kzre.homunculus.internal.utils :as u]
    [top.kzre.homunculus.version :as version]))

(defn- lookup-compile-target [target]
  (case target
    :hlsl hlsl/hlsl-target
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
      (println (str "Homunculus Compiler v" version/version))
      (System/exit 0))
    (when (empty? files)
      (println "错误: 未指定输入文件")
      (println (opts/usage-string))
      (System/exit 1))

    (let [config (model/make-compile-config options)
          target (lookup-compile-target (p/target config))
          context  (model/make-compile-context config target)
          compiler (p/compiler context)]

      ;; 编译所有输入文件
      (doseq [file-path files]
        (let [src   (slurp file-path)
              forms (u/parse-forms src)]
          (p/compile compiler forms context )))

      ;; 输出
      (if (:split-modules options)
        ;; 分割输出：每个模块单独生成文件，使用配置的命名风格
        (let [all-units (p/all-module-units context)]   ;; 从配置获取风格
          (doseq [unit all-units]
            (when-let [result     (p/compile-module compiler unit context)]
              (p/write-result result))))
        ;; 合并输出（单文件链接）
        (let [code (p/link compiler context)]
          (println code))))))
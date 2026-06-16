(ns top.kzre.homunculus.cli.core
  "最简单的 CLI 入口 —— 编译单个 Clojure 文件并输出 HLSL 代码。"
  (:gen-class)
  (:require [top.kzre.homunculus.backend.hlsl.config :as hlsl]
            [top.kzre.homunculus.internal.model :as model]
            [top.kzre.homunculus.internal.protocol :as p]
            [top.kzre.homunculus.internal.utils :as u]))

(defn -main
  [file-path & _]
  (let [src        (slurp file-path)
        forms      (u/parse-forms src)
        compiler   (hlsl/->HLSLCompiler)
        ;; 创建一个简单的、不处理依赖的编译上下文
        config     (model/->CompileConfig ["src"] ["lib"] "out")
        state      (atom {:compiling #{} :modules {}})
        context    (model/->DefaultCompileContext config compiler state)
        code     (p/emit compiler forms context)
        ]
    (println code)))
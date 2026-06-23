(ns build
      (:require [clojure.tools.build.api :as b]))

(def lib 'top.kzre/homunculus)
(def version "0.1.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean [_]
      (b/delete {:path "target"}))

(defn compile-java [_]
      (b/javac {:src-dirs ["src"]
                :class-dir class-dir
                :basis basis
                :javac-opts ["-source" "8" "-target" "8"]}))

;; 编译 src 下的 Clojure 源文件（标准库已移至 resources，不会被编译）
(defn compile-clj [_]
      (b/compile-clj {:basis basis
                      :src-dirs ["src"]
                      :class-dir class-dir}))

(defn jar [_]
      (clean nil)
      (b/write-pom {:class-dir class-dir
                    :lib lib
                    :version version
                    :basis basis
                    :src-dirs ["src"]
                    :scm {:url "https://github.com/your-user/homunculus"
                          :connection "scm:git:git://github.com/your-user/homunculus.git"
                          :developerConnection "scm:git:ssh://git@github.com:your-user/homunculus.git"}})
      ;; 复制 resources 目录到 classes，标准库作为资源包含
      (b/copy-dir {:src-dirs ["resources"]
                   :target-dir class-dir})
      (b/jar {:class-dir class-dir
              :jar-file jar-file}))

(defn uberjar [_]
      (clean nil)
      (compile-clj nil)
      ;; 复制 resources 到 class-dir，确保标准库等资源包含在 uberjar 中
      (b/copy-dir {:src-dirs ["resources"]
                   :target-dir class-dir})
      (b/uber {:class-dir class-dir
               :uber-file uber-file
               :basis basis
               :main 'top.kzre.homunculus.cli.core}))
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

;; 编译 src 下所有 Clojure 源文件（确保 defrecord 等生成类）
(defn compile [_]
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
      (b/copy-dir {:src-dirs ["src" "resources"]
                   :target-dir class-dir})
      (b/jar {:class-dir class-dir
              :jar-file jar-file}))

(defn uberjar [_]
      (clean nil)
      (compile nil)   ;; 先编译全部
      (b/uber {:class-dir class-dir
               :uber-file uber-file
               :basis basis
               :main 'top.kzre.homunculus.core.ir2}))
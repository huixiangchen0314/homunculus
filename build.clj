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

;; 新增：编译 HLSL IR3 命名空间，生成 .class 文件
(defn compile-hlsl [_]
      (b/compile-clj {:basis basis
                      :src-dirs ["src"]
                      :class-dir class-dir
                      :ns-compile '[top.kzre.homunculus.backends.hlsl.ir3-hlsl]}))


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
      (b/compile-clj {:basis basis
                      :src-dirs ["src"]
                      :class-dir class-dir
                      :ns-compile '[top.kzre.homunculus.core.ir2
                                    top.kzre.homunculus.backends.hlsl.ir3-hlsl]})
      (b/uber {:class-dir class-dir
               :uber-file uber-file
               :basis basis
               :main 'top.kzre.homunculus.core.ir2}))
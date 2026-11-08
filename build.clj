(ns build
  (:require
    [top.kzre.clj-build-template.simple :as tpl]))

(def params {:template (tpl/create-template)
             :artifact "homunculus"
             :version "0.1.0"
             :main 'top.kzre.homunculus.cli.core
             :src-dirs ["src" ]
             :resource-dirs ["resources"]})

(defn clean [_]
      (tpl/clean))

(defn compile-java [_]
      (tpl/compile-java params))

(defn compile-clj [_]
      (tpl/compile-clj params))

(defn jar [_]
      (tpl/jar params))

(defn uber [_]
      (tpl/uber params))
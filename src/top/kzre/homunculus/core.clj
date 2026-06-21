(ns top.kzre.homunculus.core
  "编译器核心 DSL，提供 defn, defn- 等语法糖。"
  (:refer-clojure :exclude [defn defn-])
  (:require [clojure.core]))

(defmacro defn
  "定义带类型标注的函数，语法与 Clojure 的 defn 类似。
   用法：
     (defn name docstring? [^type param ...] body)
     (defn name [^type param ...] body)"
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (rest args)]
                           [nil args])
        [params body]    (if (vector? (first args))
                           [(first args) (rest args)]
                           (throw (IllegalArgumentException. "Parameter vector expected")))]
    `(def ~name
       ~(if docstring
          `(fn ~(vary-meta name assoc :doc docstring) ~params ~@body)
          `(fn ~params ~@body)))))

(defmacro defn-
  "与 defn 相同，但生成的函数为私有（如有需要可扩展）。"
  [name & args]
  `(defn ~name {:private true} ~@args))
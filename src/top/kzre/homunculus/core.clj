(ns top.kzre.homunculus.core
  "编译器核心 DSL，提供 defn, defn- 等语法糖。"
  (:refer-clojure :exclude [defn defn-])
  (:require [clojure.core]))

(defmacro defn
  "定义带类型标注的函数，支持 docstring 和属性映射。"
  [name & args]
  (let [[docstring args] (if (string? (first args))
                           [(first args) (rest args)]
                           [nil args])
        ;; 跳过可选的属性映射 (如 {:private true})
        [attr-map args] (if (map? (first args))
                          [(first args) (rest args)]
                          [nil args])
        [params body]    (if (vector? (first args))
                           [(first args) (rest args)]
                           (throw (IllegalArgumentException. "Parameter vector expected")))]
    `(def ~name
       ~(cond-> `(fn ~params ~@body)
                docstring (vary-meta assoc :doc docstring)
                attr-map (vary-meta merge attr-map)))))

(defmacro defn-
  "与 defn 相同，但生成的函数为私有（通过 :private 元数据）。"
  [name & args]
  `(defn ~name {:private true} ~@args))
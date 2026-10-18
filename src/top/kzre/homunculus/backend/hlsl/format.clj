(ns top.kzre.homunculus.backend.hlsl.format
  (:refer-clojure :exclude [cast])
  (:require
   [top.kzre.homunculus.backend.clang.format :as clang.format]
   [top.kzre.homunculus.backend.clang.lang :as clang.lang]
   [top.kzre.homunculus.backend.format :refer [T]]))


(defn var-decl
  [& {:keys [name type init]}]
  (let [left (clang.format/var-decl
               name
               (if (string? type)
                 type
                 (clang.lang/type->str type))) ]
    (if init
      (T "${left} = ${init}")
      left)))

(defn cast
  [type expr]
  (let [type (if (string? type)
               type
               (clang.lang/type->str type))]
    (T "${type}(${expr})")))
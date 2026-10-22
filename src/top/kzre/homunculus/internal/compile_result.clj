(ns top.kzre.homunculus.internal.compile-result
  (:require
   [clojure.java.io :as io]
   [top.kzre.homunculus.internal.protocol :as proto]))


(defrecord ModuledResult [path content]
  proto/ICompileResult
  (paths [_] [path])
  (content [_ f]
    (when (= f path)
      content)))

(defn make-module-result
  [path content]
  (->ModuledResult path content))

(defn write-result [result]
  (let [paths (proto/paths result)]
    (doseq [p paths]
      (let [c (proto/content result p)
            f (io/file p)]
        (io/make-parents f)
        (spit f c)))))
(ns top.kzre.homunculus.internal.walk
  "保留元数据的 walk 实现，替代 clojure.walk。"
  (:import (clojure.lang IMapEntry IObj IRecord MapEntry)))

(defn walk
  [inner outer form]
  (let [m (meta form)
        o (cond
            (list? form) (outer (apply list (map inner form)))
            (instance? IMapEntry form)
            (outer (MapEntry/create (inner (key form)) (inner (val form))))
            (seq? form) (outer (doall (map inner form)))
            (instance? IRecord form)
            (outer (reduce (fn [r x] (conj r (inner x))) form form))
            (coll? form) (outer (into (empty form) (map inner form)))
            :else (outer form))]
    (if (instance? IObj o)
      (with-meta o m)
      o)))

(defn postwalk

  [f form]
  (walk (partial postwalk f) f form))

(defn prewalk
  [f form]
  (walk (partial prewalk f) identity (f form)))
(ns top.kzre.homunculus.internal.compile-target)



(defrecord CompileTarget [frontend backend compiler emitter])

(defn make-compile-target
  [frontend backend compiler emitter]
  (->CompileTarget frontend backend compiler emitter))
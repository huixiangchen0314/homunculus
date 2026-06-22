(ns top.kzre.homunculus.core.types.lambda-elim.methods.try
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.lambda-elim.core :as elim]))

(defmethod elim/eliminate :try [node config env]
  (let [[new-body body-defs] (elim/eliminate (n/try-body node) config env)
        [new-catches catches-defs]
        (reduce (fn [[catches defs] c]
                  (let [[new-c c-defs] (elim/eliminate c config env)]
                    [(conj catches new-c) (into defs c-defs)]))
                [[] []]
                (n/try-catches node))
        [new-finally finally-defs] (if-let [f (n/try-finally node)]
                                     (elim/eliminate f config env)
                                     [nil []])]
    [(n/make-try new-body new-catches new-finally
                 (n/attrs node) (n/node-meta node) (n/parent node))
     (into body-defs (into catches-defs finally-defs))]))
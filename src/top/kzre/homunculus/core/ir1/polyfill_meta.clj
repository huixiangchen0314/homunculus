(ns top.kzre.homunculus.core.ir1.polyfill-meta
  (:require
    [top.kzre.homunculus.core.ir1.ast :as ir1]))

(defrecord Env [line
                column]
  )

(defn make-env
  []
  (->Env 1 1))

(declare walk)
(defn poly-fn
  [node env]
  (let [metadata (or (ir1/node-meta node) {})
        env1 (if-let [l (:line metadata)]
              (assoc env :line l)
              env)
        env2 (if-let [c (:column metadata)]
              (assoc env1 :column c)
              env)
        l (:line env2)
        c (:column env2)
        new-metadata (assoc metadata
                       :line l
                       :column c)]
    (walk (assoc node :meta new-metadata) env2)))

(defn walk
  [node env]
  (ir1/reduce-children node poly-fn env))

(defn polyfill-nodes
  [nodes]
  (let [env (make-env)]
    (mapv #(first (poly-fn % env)) nodes)))
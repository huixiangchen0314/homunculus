(ns top.kzre.homunculus.backend.glsl.adapt
  "适配ShaderAST")

(defrecord Env [ctx])
(defn make-env [ctx]
  (->Env ctx))



(defn adapt-nodes
  [nodes ctx]
  (let [init-env (make-env ctx)]
    ))
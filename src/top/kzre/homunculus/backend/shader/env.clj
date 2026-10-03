(ns top.kzre.homunculus.backend.shader.env)

(defprotocol IEnv
  (bind-var [this var-name])
  (local-var? [this var-name]))

(defrecord Env [locals]
  IEnv
  (bind-var [this var-name] (update this :locals conj var-name))
  (local-var? [this var-name] (contains? (:locals this) var-name)))

(defn make-env
  [] (->Env #{}))
(ns top.kzre.homunculus.frontend.shader.pass.mark-escape.env)

(defprotocol IEnv
  (set-return-var [_ var-name])
  (return-var? [_ var-name]))


(defrecord Env [return-var]
  IEnv
  (set-return-var [this var-name]
    (assoc this :return-var var-name))
  (return-var? [_ var-name]
    (and var-name return-var (= var-name return-var))))


(defn make-env []
  (map->Env {}))
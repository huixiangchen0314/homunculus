(ns top.kzre.homunculus.core.types.typed.env)

(defn extend-env [env name ty] (assoc env name ty))
(defn lookup-env [env name] (get env name))
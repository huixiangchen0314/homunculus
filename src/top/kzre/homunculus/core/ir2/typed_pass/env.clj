(ns top.kzre.homunculus.core.ir2.typed-pass.env)

(defn extend-env [env name ty] (assoc env name ty))
(defn lookup-env [env name] (get env name))
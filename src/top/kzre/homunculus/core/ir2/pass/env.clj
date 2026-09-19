(ns top.kzre.homunculus.core.ir2.pass.env
  "类型推导共享环境。")

(defn extend-env [env name ty] (assoc env name ty))
(defn lookup-env [env name] (get env name))

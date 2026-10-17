(ns top.kzre.homunculus.backend.shader.pass.array-allocate.reallocate)



(defn- reallocate-node [node])

(defn reallocate [nodes]
  (mapv reallocate-node nodes))
(ns top.kzre.homunculus.frontend.shader.pass.array-allocate.reallocate)



(defn- reallocate-node [node])

(defn reallocate [nodes]
  (mapv reallocate-node nodes))
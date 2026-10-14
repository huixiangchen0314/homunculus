(ns top.kzre.homunculus.backend.shader.pass.array-allocate
  "数组分配优化pass")


(defn- optimize-node [node])

(defn optimize [nodes]
  (mapv optimize-node nodes))
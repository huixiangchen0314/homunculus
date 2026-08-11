(ns top.kzre.homunculus.backend.shader.metadata
  "着色器元数据访问辅助")

(defn shader-stage [node]
  (:shader/stage (:meta node)))

(defn shader-entry? [node]
  (:shader/entry? (:meta node)))

(defn shader-resource-kind [node]
  (:shader/resource-kind (:meta node)))

(defn shader-texture-register [node]
  (:shader/texture-register (:meta node)))

(defn shader-sampler-register [node]
  (:shader/sampler-register (:meta node)))

(defn shader-cbuffer-register [node]
  (:shader/cbuffer-register (:meta node)))

(defn shader-cbuffer-members [node]
  (:shader/cbuffer-members (:meta node)))

(defn shader-uniform? [node]
  (:shader/uniform? (:meta node)))

(defn shader-static-var? [node]
  (:shader/static-var? (:meta node)))

(defn shader-ignore-emit? [node]
  (:shader/ignore-emit? (:meta node)))
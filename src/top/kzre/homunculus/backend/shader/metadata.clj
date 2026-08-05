(ns top.kzre.homunculus.backend.shader.metadata
  "着色器元数据访问辅助"
  (:require [top.kzre.homunculus.core.ir2.ast :as ir2]))

(defn shader-stage [node]
  (:shader/stage (ir2/node-meta node)))

(defn shader-entry? [node]
  (:shader/entry? (ir2/node-meta node)))

(defn shader-resource-kind [node]
  (:shader/resource-kind (ir2/node-meta node)))

(defn shader-texture-register [node]
  (:shader/texture-register (ir2/node-meta node)))

(defn shader-sampler-register [node]
  (:shader/sampler-register (ir2/node-meta node)))

(defn shader-cbuffer-register [node]
  (:shader/cbuffer-register (ir2/node-meta node)))

(defn shader-cbuffer-members [node]
  (:shader/cbuffer-members (ir2/node-meta node)))

(defn shader-uniform? [node]
  (:shader/uniform? (ir2/node-meta node)))

(defn shader-static-var? [node]
  (:shader/static-var? (ir2/node-meta node)))

(defn shader-ignore-emit? [node]
  (:shader/ignore-emit? (ir2/node-meta node)))
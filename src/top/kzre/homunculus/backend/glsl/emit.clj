(ns top.kzre.homunculus.backend.glsl.emit
  (:require [top.kzre.homunculus.backend.shader.ast :as ast])
  (:import (top.kzre.homunculus.backend.shader.pass.analyze.model SLAnalysis)))



(defmulti emit-node* (fn [node _env] (ast/kind node)))

(defn emit [nodes ^SLAnalysis analysis])
(ns top.kzre.homunculus.backend.glsl.sematic.core
  (:require
    [top.kzre.homunculus.backend.glsl.sematic.layout]
    [top.kzre.homunculus.backend.shader.analyze])
  (:import
    (top.kzre.homunculus.backend.glsl.sematic.layout Layout)
    (top.kzre.homunculus.backend.shader.analyze SLAnalysis)))

(defn lower
  "对 shader ast 进行语义降级，生成数据规划"
 ^Layout [^SLAnalysis anaysis cfg])
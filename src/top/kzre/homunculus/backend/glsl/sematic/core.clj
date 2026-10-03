(ns top.kzre.homunculus.backend.glsl.sematic.core
  (:import (top.kzre.homunculus.backend.glsl.sematic.layout Layout)
           (top.kzre.homunculus.backend.shader.analyze Analysis)))


(defn lower
  "对 shader ast 进行语义降级，生成数据规划"
 ^Layout [^Analysis anaysis cfg])
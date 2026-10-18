(ns top.kzre.homunculus.backend.glsl.sematic.location
  "默认的语义布局"
  )


(def ^:private default-location*
  {:position 0
   :normal 1
   :texcoord0 0})

(defn default-location [semantic]
  (default-location* semantic))
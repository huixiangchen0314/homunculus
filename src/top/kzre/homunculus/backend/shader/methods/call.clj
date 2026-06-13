(ns top.kzre.homunculus.backend.shader.methods.call
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]))

(defmethod emit :call [node backend]
  (let [fn-node (:fn node)
        args    (:args node)
        fn-name (emit fn-node backend)
        arg-strs (map #(emit % backend) args)]
    (sp/shader-call backend fn-name arg-strs)))
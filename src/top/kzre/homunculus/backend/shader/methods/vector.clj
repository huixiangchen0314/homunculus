(ns top.kzre.homunculus.backend.shader.methods.vector
  (:require [top.kzre.homunculus.backend.shader.emit :refer [emit]]
            [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.types.model :as t]
            [clojure.string :as str]))

(defmethod emit :vector [node backend]
  (let [items (:items node)
        elem-type (or (some-> (first items) (get-in [:attrs :type]))
                      (t/->TCon :float))
        elem-name (name (:name elem-type))
        type-str (str elem-name (count items))
        args (map #(emit % backend) items)]
    (str type-str "(" (str/join ", " args) ")")))
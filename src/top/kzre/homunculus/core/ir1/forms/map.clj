(ns top.kzre.homunculus.core.ir1.forms.map
  (:require [top.kzre.homunculus.core.ir1.core :as ir1]
            [top.kzre.homunculus.core.ir1.node :as n]))

(defmethod ir1/form->node :map [form]
  (let [pairs (mapcat (fn [[k v]] [k v]) form)]
    (n/make-map (vec pairs) (meta form))))

(defmethod ir1/build-tree :map [node]
  (n/make-map (mapv ir1/->ir1 (n/map-pairs node))
              (n/node-meta node)
              (n/parent node)))
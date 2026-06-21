(ns top.kzre.homunculus.core.types.recur-elim.methods.array
  (:require [top.kzre.homunculus.core.ir2.node :as n]
            [top.kzre.homunculus.core.types.recur-elim.core :as rec]))

(defmethod rec/eliminate :new-array [node]
  (n/make-new-array (rec/eliminate (n/new-array-size node))))

(defmethod rec/eliminate :aget [node]
  (n/make-aget (rec/eliminate (n/aget-target node))
               (rec/eliminate (n/aget-idx node))))

(defmethod rec/eliminate :aset [node]
  (n/make-aset (rec/eliminate (n/aset-target node))
               (rec/eliminate (n/aset-idx node))
               (rec/eliminate (n/aset-val node))))

(defmethod rec/eliminate :alength [node]
  (n/make-alength (rec/eliminate (n/alength-target node))))

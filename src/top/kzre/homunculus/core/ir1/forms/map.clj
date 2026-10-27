(ns top.kzre.homunculus.core.ir1.forms.map
  (:require
    [top.kzre.homunculus.core.ir1.core :as ir1]
    [top.kzre.homunculus.core.ir1.ast :as m]))

(defmethod ir1/lower-node* :map [form env]
  (let [meta-  (meta form)
        [pairs env']
        (reduce (fn [[acc e] [k v]]
                  (let [[k' e']  (ir1/lower-node k e)
                        [v' e''] (ir1/lower-node v e')]
                    [(conj acc (m/->Pair k' v' meta-)) e'']))
                [[] env]
                form)]
    [(m/->Map pairs meta-) env']))
(ns top.kzre.homunculus.core.types.ho-elim.methods.map
  "消除 (map f coll) —— 仅处理字面量向量（静态长度）。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]))

(defn expand-map [f coll]
  (if (= (n/kind coll) :vector)
    (let [items (n/vector-items coll)
          new-items (mapv (fn [item] (n/make-call f [item] {} nil nil)) items)]
      (n/make-vector new-items nil nil nil))
    (throw (ex-info "map currently only supports literal vectors" {:coll coll}))))
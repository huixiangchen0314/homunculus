(ns top.kzre.homunculus.core.types.ho-elim.methods.reduce
  "消除 (reduce f init coll) —— 仅处理字面量向量（静态长度）。"
  (:require [top.kzre.homunculus.core.ir2.node :as n]))

(defn expand-reduce [f init coll]
  (if (= (n/kind coll) :vector)
    (let [items (n/vector-items coll)]
      (if (empty? items)
        init
        (reduce (fn [acc item] (n/make-call f [acc item] {} nil nil))
                init items)))
    (throw (ex-info "reduce currently only supports literal vectors" {:coll coll}))))
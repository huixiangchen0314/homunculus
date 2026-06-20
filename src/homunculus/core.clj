(ns homunculus.core
  "homunculus 核心标准库. 一般以内联或单态化的形式被一同编译到目标平台."
  (:refer-clojure :exclude [ map reduce]))

(defn map [f coll]
  (if (empty? coll)
    []
    (let [x (first coll)
          xs (rest coll)]
      (conj (map f xs) (f x)))))    ;; 注意：conj 加到末尾保持顺序，需要优化

(defn reduce [f init coll]
  (if (empty? coll)
    init
    (let [x (first coll)
          xs (rest coll)]
      (reduce f (f init x) xs))))
(ns cljh.core
  "Clojure DSL 编译时标准库，所有函数在编译时静态展开.")

(defn map [f coll]
  (let [n (%%alength coll)
        arr (%%new-array n)]
    (loop [i 0]
      (if (%%< i n)
        (do (%%aset arr i (f (%%aget coll i)))
            (recur (%%+ i 1)))
        arr))))

(defn reduce [f init coll]
  (let [n (%%alength coll)]
    (loop [i 0, acc init]
      (if (%%< i n)
        (recur (%%+ i 1) (f acc (%%aget coll i)))
        acc))))

(defn ^:inline conj [coll x]
  (let [n (%%alength coll)
        new-arr (%%new-array (%%+ n 1))]
    (loop [i 0]
      (if (%%< i n)
        (do (%%aset new-arr i (%%aget coll i))
            (recur (%%+ i 1)))
        (do (%%aset new-arr n x)
            new-arr)))))

;; 对静态类型不合法
(defn filter [pred coll]
  (let [n (%%alength coll)]
    (loop [i 0, acc (%%new-array 0)]
      (if (%%< i n)
        (let [x (%%aget coll i)]
          (if (pred x)
            (recur (%%+ i 1) (conj acc x))
            (recur (%%+ i 1) acc)))
        acc))))
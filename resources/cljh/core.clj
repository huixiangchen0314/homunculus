(ns cljh.core
  "Clojure DSL 编译时标准库，所有函数在编译时静态展开.")

(defn ^:inline count [coll]
  (%%alength coll))

(defn ^:inline first [coll]
  (%%aget coll 0))

(defn ^:inline last [coll]
  (%%aget coll (%%- (%%alength coll) 1)))

(defn ^:inline butlast [coll]
  (let [len (%%alength coll)
        new-len (%%- len 1)
        arr (%%new-array new-len)]
    (loop [i 0]
      (if (%%< i new-len)
        (do (%%aset arr i (%%aget coll i))
            (recur (%%+ i 1)))
        arr))))

(defn ^:inline nth [coll idx]
  (%%aget coll idx))

(defn ^:inline range [n]
  (let [arr (%%new-array n)]
    (loop [i 0]
      (if (%%< i n)
        (do (%%aset arr i i)
            (recur (%%+ i 1)))
        arr))))

(defn ^:inline repeat [n x]
  (let [arr (%%new-array n)]
    (loop [i 0]
      (if (%%< i n)
        (do (%%aset arr i x)
            (recur (%%+ i 1)))
        arr))))

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

(defn map-indexed [f coll]
  (let [n (%%alength coll)
        arr (%%new-array n)]
    (loop [i 0]
      (if (%%< i n)
        (do (%%aset arr i (f i (%%aget coll i)))
            (recur (%%+ i 1)))
        arr))))


(defn ^:inline reverse [coll]
  (let [n (%%alength coll)
        arr (%%new-array n)]
    (loop [i 0]
      (if (%%< i n)
        (do (%%aset arr i (%%aget coll (%%- (%%- n 1) i)))
            (recur (%%+ i 1)))
        arr))))


(defn ^:inline concat [a b]
  (let [n1 (%%alength a)
        n2 (%%alength b)
        n  (%%+ n1 n2)
        arr (%%new-array n)]
    ;; 拷贝 a
    (loop [i 0]
      (if (%%< i n1)
        (do (%%aset arr i (%%aget a i))
            (recur (%%+ i 1)))
        ;; 拷贝 b
        (loop [j 0]
          (if (%%< j n2)
            (do (%%aset arr (%%+ n1 j) (%%aget b j))
                (recur (%%+ j 1)))
            arr))))))


;; 取前 k 个元素（k 必须为编译期常量，且 k <= n）
(defn ^:inline take [k coll]
  (let [arr (%%new-array k)]
    (loop [i 0]
      (if (%%< i k)
        (do (%%aset arr i (%%aget coll i))
            (recur (%%+ i 1)))
        arr))))

;; 丢弃前 n 个元素（n 必须为编译期常量，且 n <= len）
(defn ^:inline drop [n coll]
  (let [len (%%alength coll)
        new-len (%%- len n)
        arr (%%new-array new-len)]
    (loop [i 0]
      (if (%%< i new-len)
        (do (%%aset arr i (%%aget coll (%%+ i n)))
            (recur (%%+ i 1)))
        arr))))

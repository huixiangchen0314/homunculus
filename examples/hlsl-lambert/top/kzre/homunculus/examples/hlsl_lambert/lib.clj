(ns top.kzre.homunculus.examples.hlsl-lambert.lib)


;; ── 用户自定义高阶函数 (测试用) ──────────
(defn my-map [f coll]
  (let [n (%%alength coll)
        arr (%%new-array n)]
    (loop [i 0]
      (if (< i n)
        (do (%%aset arr i (f (%%aget coll i)))
            (recur (+ i 1)))
        arr))))

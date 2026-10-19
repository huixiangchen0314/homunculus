(ns top.kzre.homunculus.backend.format
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.internal.utils :as iu]))


(defmacro T [template-str]
  (let [re   #"\$\{([^}]+)\}"
        parts (str/split template-str re -1)        ;; 保留尾部空串，确保静态部分完整
        exprs (map (comp symbol second) (re-seq re template-str))]
    `(str ~@(loop [p parts, e exprs, result []]
              (if (empty? p)
                result
                (let [s (first p)
                      expr (first e)]
                  (recur (rest p)
                         (rest e)
                         (conj result s (when expr expr)))))))))


(defn rows [rows]
  (str/join "\n" rows))

(defn ensure-suffix
  "确保字符串 s 以 suffix 结尾，没有则补上。"
  [s suffix]
  (iu/ensure-suffix s suffix))


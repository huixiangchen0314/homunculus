(ns top.kzre.homunculus.backend.format
  (:require [clojure.string :as string]))



(defmacro T [template-str]
  (let [re   #"\$\{([^}]+)\}"
        parts (string/split template-str re -1)        ;; 保留尾部空串，确保静态部分完整
        exprs (map (comp symbol second) (re-seq re template-str))]
    `(str ~@(loop [p parts, e exprs, result []]
              (if (empty? p)
                result
                (let [s (first p)
                      expr (first e)]
                  (recur (rest p)
                         (rest e)
                         (conj result s (when expr expr)))))))))
(ns top.kzre.homunculus.backend.util.naming
  "通用名称处理：字符转义。保留字处理由各后端自行负责。"
  (:require
    [clojure.string :as str]))

(def ^:private char-replacements
  "将 Clojure 特殊字符映射为安全的 C 标识符片段。"
  {\! "_bang_"
   \? "_p_"
   \* "_star_"
   \+ "_plus_"
   \- "_"       ; 连字符转为下划线
   \_ "_"       ; 下划线保留
   \$ "_dollar_"
   \% "_pct_"
   \& "_amp_"
   \= "_eq_"
   \< "_lt_"
   \> "_gt_"
   \. "_"       ; 点转为下划线
   \# "_hash_"
   \: "_"       ; 冒号转为下划线
   \/ "_"       ; 斜线转为下划线
   \@ "_at_"
   \' "_quote_"})

(defn safe-name
  "将符号或字符串转为安全的 C 标识符。
   Clojure 特殊字符按映射表替换，其余非法字符替换为 '_'。"
  [sym-or-str]
  (let [s (name sym-or-str)]
    (-> s
        ;; 先替换映射表中的字符
        (str/replace (re-pattern (str "[" (apply str (keys char-replacements)) "]"))
                     (fn [ch] (get char-replacements (first ch) "_")))
        ;; 再将其它非法字符替换为下划线
        (str/replace #"[^a-zA-Z0-9_]" "_"))))
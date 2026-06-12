(ns top.kzre.homunculus.backend.util.naming
  "通用名称处理：字符转义。保留字处理由各后端自行负责。")

(defn safe-name
  "将符号或字符串转为安全的标识符：非法字符替换为 '_'。"
  [sym-or-str]
  (-> sym-or-str name (clojure.string/replace #"[^a-zA-Z0-9_]" "_")))
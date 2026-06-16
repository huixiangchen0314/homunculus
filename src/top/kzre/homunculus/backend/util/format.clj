(ns top.kzre.homunculus.backend.util.format
  "缩进、字符串拼接等排版工具。"
  (:require
   [clojure.string :as string]))

(defmacro tmpl [template-str]
  ;; 解析字符串中的 ${...} 部分
  (let [parts (re-seq #"([^$]*)\$\{([^}]+)\}|(.+)" template-str)]
    `(str
       ~@(mapcat
           (fn [[_ prefix expr suffix]]
             (cond
               expr   [(when (seq prefix) prefix) (list 'identity (read-string expr))]
               suffix [(or suffix "")]))
           parts))))

(defn indent
  "返回 n 级缩进字符串（每级 4 空格）。"
  [level]
  (apply str (repeat (* 4 level) \space)))

(defn lines
  "用换行符连接多个字符串，自动过滤 nil。"
  [& strs]
  (string/join "\n" (remove nil? strs)))

(defn comma-sep
  "用逗号加空格连接字符串序列。"
  [items]
  (string/join ", " items))

(defn parens
  "给字符串加圆括号。"
  [s]
  (str "(" s ")"))

(defn braces
  "给字符串加大括号，可选缩进内部内容。"
  ([s]
   (str "{\n" s "\n}"))
  ([s level]
   (let [ind (indent level)]
     (str ind "{\n" s "\n" ind "}"))))
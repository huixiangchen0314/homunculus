(ns top.kzre.homunculus.backend.indent
  "文本缩进工具。语言无关。"
  (:require
    [clojure.string :as str]))

(def ^:dynamic *indent-size*
  "缩进的空格数。默认 4。可通过 binding 覆盖——例如按目标语言的
   风格调整（HLSL / GLSL 4，K&R C 4，GNU C 2）。"
  4)

(defn indent
  "为 text 的每一行添加缩进前缀。空文本返回空字符串。"
  [text]
  (if (str/blank? text)
    ""
    (let [prefix (apply str (repeat *indent-size* \space))]
      (->> (str/split text #"\n")
           (map #(str prefix %))
           (str/join "\n")))))
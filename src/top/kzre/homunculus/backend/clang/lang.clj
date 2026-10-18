(ns top.kzre.homunculus.backend.clang.lang
  "C 风格语言的通用工具——字面量、操作符。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.format :refer [T]]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

(defn literal->str
  "字面量值 → C 风格字面量字符串。
   nil 返回 nil——表示无字面量。"
  [val]
  (cond
    (nil? val)     nil
    (integer? val) (str val)
    (float? val)   (str val)
    (true? val)    "true"
    (false? val)   "false"
    :else
    (throw (ex-info "unsupported literal" {:val val}))))


(def ^:private naming-char-map
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

(defn naming
  "将符号或字符串转为安全的 C 标识符。
   Clojure 特殊字符按映射表替换，其余非法字符替换为 '_'。"
  ([sym-or-str] (naming sym-or-str true))
  ([sym-or-str strip-ns?]
   (let [s (if (or strip-ns?
                   (string? sym-or-str)
                   (and (symbol? sym-or-str)
                        (nil? (namespace sym-or-str))))
             (name sym-or-str)
             (str (namespace sym-or-str) "_" (name sym-or-str)))]
     (-> s
         ;; 先替换映射表中的字符
         (str/replace (re-pattern (str "[" (apply str (keys naming-char-map)) "]"))
                      (fn [ch] (get naming-char-map (first ch) "_")))
         ;; 再将其它非法字符替换为下划线
         (str/replace #"[^a-zA-Z0-9_]" "_")))))

(defn type->str
  "ir类型转C风格类型字符串，默认严格，数组长度必须编译时已知"
  ([ir-type] (type->str ir-type true))
  ([ir-type strict?]
   (cond
     (ty/vec-type? ir-type)
     (let [elem-type (type->str (ty/vec-element-type ir-type) strict?)
           size (ty/value-val (ty/vec-size ir-type))]
       (T "${elem-type}[${size}]"))
     (ty/type-sym ir-type) (naming (ty/type-sym ir-type))
     :else (if strict?
             (throw (ex-info (str "Unknown type: " ir-type) {}))
             nil))))

(def ^:private op-naming-map
  "Clojure 风格运算符 → C 风格运算符字符串。"
  {'+              "+"
   '-              "-"
   '*              "*"
   '/              "/"
   'mod            "%"

   '=              "=="
   'not=           "!="
   '<              "<"
   '>              ">"
   '<=             "<="
   '>=             ">="

   'and            "&&"
   'or             "||"
   'not            "!"

   'bit-and        "&"
   'bit-or         "|"
   'bit-xor        "^"
   'bit-not        "~"
   'bit-shift-left  "<<"
   'bit-shift-right ">>"})

(defn naming-operator
  "把 Clojure 风格的操作符符号换成 C 风格的运算符字符串。
   未知运算符返回 nil。"
  [op-sym]
  (get op-naming-map op-sym))
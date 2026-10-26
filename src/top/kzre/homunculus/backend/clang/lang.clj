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

(def ^:dynamic *naming* naming)

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


;; ── 运算符优先级 ──────────────────────────────────────
;; key 是 naming-operator 输出的 C 风格运算符字符串。
;; 数值越大优先级越高。未登记的 op 按 0 处理。

(def ^:private operator-precedence
  {"||" 1
   "&&" 2
   "|"  3
   "^"  4
   "&"  5
   "==" 6 "!=" 6
   "<"  7 ">"  7 "<=" 7 ">=" 7
   "<<" 8 ">>" 8
   "+"  9 "-"  9
   "*"  10 "/" 10 "%" 10})


(def ^:private associative-ops
  "结合律成立的运算符——同级右侧子表达式不需要括号。"
  #{"+" "*" "&&" "||" "&" "|" "^"})

(def ^:private unary-precedence
  "一元运算符优先级——高于所有二元。"
  12)

(defn operator-precedence-of
  "C 风格运算符字符串 → 优先级数值。未登记返回 0。"
  [op-str]
  (get operator-precedence op-str 0))

(defn unary-operator-precedence
  "一元运算符优先级。"
  []
  unary-precedence)

(defn operand-needs-parens?
  "判断 child-op 作为 parent-op 的操作数是否需要括号。

   child-op / parent-op 都是 naming-operator 的输出字符串。
   child-op 为 nil 表示原子表达式（字面量、变量、调用、构造器）
   ——永远不需要括号。

   right-side? —— 是否右侧子节点。左结合运算符下，同级右侧需要
                  括号（a - (b - c)）；结合律成立的运算符不需要
                  （a + b + c ≡ a + (b + c)）。"
  [child-op parent-op right-side?]
  (if (nil? child-op)
    false
    (let [child-op (name child-op)
          parent-op (name parent-op)
          parent-prec (operator-precedence-of parent-op)
          child-prec  (operator-precedence-of child-op)]
      (cond
        (< child-prec parent-prec) true
        (> child-prec parent-prec) false
        :else (and right-side?
                   (not (contains? associative-ops parent-op)))))))

(defn unary-operand-needs-parens?
  "一元运算符作用于 child-op 时是否需要括号。

   一元高于任何二元——只有一元作用于二元（或同级一元）时需要考虑。
   当前只对二元子表达式加括号。"
  [child-op]
  (some? child-op))
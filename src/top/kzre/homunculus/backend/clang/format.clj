(ns top.kzre.homunculus.backend.clang.format
  "C 风格语言的声明格式化——数组声明等。"
  (:refer-clojure :exclude [struct while])
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.clang.lang :as lang]
    [top.kzre.homunculus.backend.format :refer [T]]))



(defn- split-array-type
  "把 \"T[N]\" 形式的类型字符串拆为 [T N]。
   非数组形式返回 [type-str nil]。"
  [type-str]
  (if-let [m (re-matches #"^(.+)\[(\d+)\]$" type-str)]
    [(nth m 1) (nth m 2)]
    [type-str nil]))

(defn var-decl
  [var-name type-str & {:keys [static? const?]}]
  (let [[base size] (split-array-type type-str)
        prefix      (cond-> ""
                            static? (str "static ")
                            const?  (str "const "))
        decl        (if size
                      (str base " " var-name "[" size "]")
                      (str type-str " " var-name))]
    (str prefix decl)))



(defn binary-op
  [op left right & {:keys [left-parens? right-parens?]}]
  (let [left  (if left-parens?  (str "(" left ")")  left)
        right (if right-parens? (str "(" right ")") right)]
    (T "${left} ${op} ${right}")))

(defn unary-op
  [op expr & {:keys [operand-parens?]}]
  (let [expr (if operand-parens? (str "(" expr ")") expr)]
    (T "${op}${expr}")))

(defn assign
  [lhs rhs]
  (T "${lhs} = ${rhs}"))

(defn array-index
  [target index]
  (T "${target}[${index}]"))

(defn member-access
  [target member]
  (T "${target}.${member}"))

(defn call
  [op args]
  (str op "(" (str/join ", " args) ")"))

(defn include
  [path & {:keys [std?]}]
  (if std?
    (T "#include <${path}>")
    (T "#include \"${path}\"")))

(defn if [& {:keys [test then else]}]
  (if else
    (T "if (${test})\n{\n${then}\n}\nelse\n{\n${else}\n}")
    (T "if (${test})\n{\n${then}\n}")))

(defn while
  [test body]
  (T "while (${test})\n{\n${body}\n}"))

(defn block [body]
  (T "{\n${body}\n}"))

(defn function
  [& {:keys [name return-type params body
             before after]}]
  (let [return-type return-type
        params     (str/join ", " params)
        body       body]
    (cond
      (and before after)
      (T "${before} ${return-type} ${name}(${params}) ${after} {\n${body}\n}")
      before
      (T "${before} ${return-type} ${name}(${params}) {\n${body}\n}")
      after
      (T "${return-type} ${name}(${params}) ${after} {\n${body}\n}")
      :else
      (T "${return-type} ${name}(${params}) {\n${body}\n}"))))

(defn struct
  [& {:keys [name members instance]}]
  (if instance
    (T "struct ${name}\n{\n${members}\n} ${instance};")
    (T "struct ${name}\n{\n${members}\n};")))

(defn struct-member
  [& {:keys [name type before after post]}]
  (str (when before (str before " "))
       type " " name
       (when after (str " " after))
       ";"
       (when post (str " " post))))

(defn construct
  [type args]
  (let [args (str/join ", " args)]
    (T "${type}(${args})")))

(defn array-construct
  [args]
  (let [args (str/join ", " args)]
    (T "{${args}}")))

(defn param
  [& {:keys [type name before after]}]
  (str (when before (str before " "))
       type " " name
       (when after (str " " after))))


(defn forhead
  "for 循环头——`for (init; test; update)`。

   init / update 为 nil 时输出空段——`for (; test;)`。
   body 由调用方拼接——forhead 只负责头部。"
  [& {:keys [init test update]}]
  (let [init   (or init "")
        test   (or test "")
        update (or update "")]
    (T "for (${init}; ${test}; ${update})")))

(defn loopfor
  "for 语句——头部 + body。

   body 已由调用方 indent——此函数只负责外层 `{}` 包裹。"
  [& {:keys [init test update body]}]
  (let [head (forhead :init init :test test :update update)]
    (T "${head}\n{\n${body}\n}")))
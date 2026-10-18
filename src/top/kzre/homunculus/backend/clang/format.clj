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
  (let [var-name (lang/naming var-name)
        [base size] (split-array-type type-str)
        prefix      (cond-> ""
                            static? (str "static ")
                            const?  (str "const "))
        decl        (if size
                      (str base " " var-name "[" size "]")
                      (str type-str " " var-name))]
    (str prefix decl)))



(defn binary-op
  [op left right]
  (let [op    (lang/naming-operator op)]
    (T "${left} ${op} ${right}")))


(defn unary-op
  [op expr]
  (let [op   (lang/naming-operator op)]
    (T "${op}${expr}")))

(defn assign
  [lhs rhs]
  (T "${lhs} = ${rhs}"))

(defn array-index
  [target index]
  (T "${target}[${index}]"))

(defn member-access
  [target member]
  (let [member (lang/naming member)]
    (T "${target}.${member}")))

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
  (let [name       (lang/naming name)
        return-type return-type
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
       type " " (lang/naming name)
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
       type " " (lang/naming name)
       (when after (str " " after))))
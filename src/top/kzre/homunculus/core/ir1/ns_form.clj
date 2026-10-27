(ns top.kzre.homunculus.core.ir1.ns-form
  "ns 形式解析。

  解析形如下的 ns 表单：
     (ns cljh.shader.cgtk
       \"docstring\"
       (:require [a :as a] [b :as b])
       (:require [b :as b])
       (:require-macros [m :as m]))

   `parse-form` 返回 `NsForm` 记录：
     {:ns-sym     cljh.shader.cgtk          ; symbol
      :docstring  \"docstring\"              ; string，缺省 \"\"
      :references {:require        ([a :as a] [b :as b])   ; vector of libspec
                   :require-macros ([m :as m])
                   ...}}                     ; key 为 keyword，value 为 vector

   同名 clause（多个 `:require`）的 libspec 按顺序合并，不覆盖。")

(defrecord NsForm
  [ns-sym docstring references])

(defn reference-entry
  "按 clause 关键字返回 libspec 向量。

   kind 如 :require / :require-macros / :use。
   返回 vector of libspec（如 `[[a :as a] [b :as b]]`），
   该 kind 不存在时返回 nil。

   示例：
     (reference-entry form :require)
     ;; => ([a :as a] [b :as b])"
  [^NsForm form kind]
  (get (:references form) kind))

(defn docstring
  "返回 ns 的 docstring 字符串；未声明时返回 \"\"。"
  [^NsForm form]
  (:docstring form))

(defn ns-sym
  "返回 ns 符号，如 `cljh.shader.cgtk`。"
  [^NsForm form]
  (:ns-sym form))

(defn- parse-references
  "把 ns 的 clause 序列合并成 {keyword [libspec...]}。

   - 同名 keyword 的多个 clause 按出现顺序合并，不覆盖
   - 每个 libspec 保持原样（`[a :as a]`、`[b.b :refer [x]]`），
     prefix list `(a [b :as b])` 不做展开

   示例：
     [(:require [a :as a] [b :as b])
      (:require [b :as b])
      (:require-macros [m :as m])]
     ;; => {:require        ([a :as a] [b :as b] [b :as b])
     ;;     :require-macros ([m :as m])}"
  [references]
  (reduce (fn [m entry]
            (let [[kind & entries] entry]
              (update m kind (fnil into []) entries)))
          {}
          references))

(defn parse-form
  "解析 ns 表单，返回 `NsForm`。

   ns-form 为已 read 的顶层表单：
     (ns my.ns \"doc\" (:require [a :as a]) (:require-macros [m :as m]))

   校验：
   - 首个元素必须是 `ns`，否则抛 ex-info {:form ns-form}
   - docstring 位置若是字符串则作为 docstring；否则 docstring 为 \"\"

   返回：NsForm {:ns-sym symbol, :docstring string, :references map}
   references 的结构见 `parse-references`。"
  [ns-form]
  (let [[decl ns* & more] ns-form]
    (when-not (= decl 'ns)
      (throw (ex-info "invalid ns-form" {:form ns-form})))
    (let [maybe-doc (first more)
          [doc references]
          (if (string? maybe-doc)
            [maybe-doc (parse-references (rest more))]
            ["" (parse-references more)])]
      (->NsForm ns* doc references))))

(defn reference-alias
  "用该 reference 去解析命名空间别名。

   reference 形如 [aa.bb :as a] / [aa.bb :refer all] / [aa.bb :refer [x y]] / [aa.bb]
   sym 为待解析的符号，可带命名空间前缀（a/ll）或不带（ll）。

   返回完全限定符号，或该 reference 不匹配时返回 nil：
     [aa.bb :as a]        + b/ll  => nil
     [aa.bb :as a]        + a/ll  => aa.bb/ll
     [aa.bb :as a]        + ll    => nil
     [aa.bb :refer all]   + ll    => aa.bb/ll
     [aa.bb :refer :all]  + ll    => aa.bb/ll
     [aa.bb :refer [x y]] + x     => aa.bb/x
     [aa.bb :refer [x y]] + z     => nil
     [aa.bb :as a :refer [x]] + a/x => aa.bb/x
     [aa.bb :as a :refer [x]] + x   => aa.bb/x
     [aa.bb]              + ll    => nil
     [aa.bb]              + a/ll  => nil"
  [reference sym]
  (let [[ns-sym & kv] reference
        opts          (apply hash-map kv)
        alias         (some-> (:as opts) name)
        refer         (get opts :refer)
        refer-all?    (or (= :all refer) (= 'all refer))
        refer-names   (when (and (coll? refer) (not refer-all?))
                        (set (map name refer)))
        sym-ns        (namespace sym)
        sym-name      (name sym)]
    (cond
      ;; 带命名空间前缀：必须是别名匹配
      sym-ns
      (when (= sym-ns alias)
        (symbol (str ns-sym) sym-name))

      ;; 无前缀且 :refer :all / :refer all
      refer-all?
      (symbol (str ns-sym) sym-name)

      ;; 无前缀且 :refer [x y] 包含该符号
      (and refer-names (contains? refer-names sym-name))
      (symbol (str ns-sym) sym-name)

      :else nil)))

(defn reference-alias-in-ns
  "在 NsForm 的指定 clause 里查找 sym 的所有匹配来源，返回完全限定符号的逆序列表。

   kind 如 :require / :require-macros / :use。
   遍历该 clause 下所有 libspec，对每个 reference 调 `reference-alias`，
   收集所有非 nil 结果并逆序返回（最近的声明在前，符合 Clojure「后声明覆盖先声明」语义）。
   kind 不存在或没有匹配时返回 ()。

   示例：
     ;; (:require [aa.bb :as a] [cc.dd :refer all])
     (reference-alias-in-ns form :require 'a/ll)   ;; => (aa.bb/ll)
     (reference-alias-in-ns form :require 'll)     ;; => (cc.dd/ll)
     ;; (:require [aa.bb :refer all] [cc.dd :refer all])
     (reference-alias-in-ns form :require 'll)     ;; => (cc.dd/ll aa.bb/ll)
     (reference-alias-in-ns form :require 'nope)   ;; => ()"
  [^NsForm form kind sym]
  (->> (reference-entry form kind)
       (keep #(reference-alias % sym))
       reverse))

(defn reference-namespace
  "[aa.bb ...] -> aa.bb"
  [reference]
  (first reference))
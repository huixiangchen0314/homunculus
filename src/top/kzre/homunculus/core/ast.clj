(ns top.kzre.homunculus.core.ast
  "抽象语法树宏工具。
   提供 defast 宏，用声明式 DSL 定义 AST 节点及其协议，
   自动生成：
   - 一个协议（含 kind, children, map-children, node-meta 方法）
   - 各节点的 defrecord 实现
   - 通用的后序/前序树遍历函数 (postwalk, prewalk)
   记录的字段顺序与 DSL 中声明的顺序严格一致（属性、子节点交错），末尾追加 meta 字段。"
  (:require [clojure.string :as str]))

(defn- kind->record-name [kind]
  (let [s (name kind)
        parts (str/split s #"-")]
    (when (re-find #"[^a-zA-Z0-9-]" s)
      (throw (ex-info (str "Invalid character in node kind: " kind) {:kind kind})))
    (symbol (apply str (map str/capitalize parts)))))

(defrecord TypeInfo [name subtypes many?])
(defrecord ChildSpec [field many?])
;; 增加 field-order 保留字段的声明顺序
(defrecord NodeDef [kind record-name attrs children-specs field-order])

(defn emit-ast-protocol [name]
  `(defprotocol ~name
     (~'kind       [~'this] "返回节点类型关键字")
     (~'children   [~'this] "返回直接子节点的向量")
     (~'map-children [~'this ~'f] "用转换函数 f 映射所有直接子节点，返回新节点")
     (~'node-meta  [~'this] "返回元数据 map")))

(defn- parse-type-map [type-map]
  (into {}
        (map (fn [[k v]]
               (let [cleaned (remove #{'/} v)
                     many?   (boolean (some #(and (map? %) (:many %)) cleaned))
                     subtypes (vec (remove map? cleaned))]
                 [k (->TypeInfo k subtypes many?)])))
        type-map))

(defn- parse-node-def [type-infos kind fields]
  (let [kind (if (keyword? kind) kind (keyword kind))
        ;; 在循环中同时收集属性、子节点规范和字段顺序
        {:keys [attrs children-specs field-order]}
        (loop [remaining fields
               attrs []
               specs []
               field-order []]
          (if (empty? remaining)
            {:attrs attrs :children-specs (vec specs) :field-order (vec field-order)}
            (let [elem (first remaining)
                  rst  (rest remaining)]
              (cond
                ;; 属性字段 'name
                (and (seq? elem) (= 'quote (first elem)))
                (let [field-name (second elem)]
                  (recur rst (conj attrs field-name) specs (conj field-order field-name)))

                ;; 子节点字段 :type 或 :type 'fieldname
                (keyword? elem)
                (let [type-key elem
                      [field-name rst']
                      (if (and (seq rst) (seq? (first rst)) (= 'quote (ffirst rst)))
                        [(second (first rst)) (rest rst)]
                        [(symbol (name type-key)) rst])
                      many? (get-in type-infos [type-key :many?] false)]
                  (recur rst' attrs (conj specs (->ChildSpec field-name many?)) (conj field-order field-name)))

                :else
                (throw (ex-info (str "Invalid field spec: " elem) {}))))))]
    (->NodeDef kind
               (kind->record-name kind)
               attrs
               children-specs
               field-order)))

(defn- parse-all-nodes [type-infos node-defs]
  (map (fn [[k v]] (parse-node-def type-infos k v))
       (partition 2 node-defs)))

(defn- emit-children-body [node-def protocol-name]
  (let [segments (for [{:keys [field many?]} (:children-specs node-def)]
                   (if many?
                     `(filter #(satisfies? ~protocol-name %) ~field)
                     `(when (satisfies? ~protocol-name ~field) [~field])))]
    `(vec (concat ~@segments))))

(defn- emit-map-children-body [node-def]
  (let [updates (for [{:keys [field many?]} (:children-specs node-def)]
                  (if many?
                    `[~field (mapv ~'f ~field)]
                    `[~field (some-> ~field ~'f)]))]
    `(assoc ~'this ~@(mapcat identity updates))))

(defn- emit-ast-node [protocol-name node-def]
  ;; 使用 field-order 保持声明顺序，末尾追加 meta
  (let [fields (conj (:field-order node-def) 'meta)]
    `(defrecord ~(:record-name node-def) ~fields
       ~protocol-name
       (~'kind [~'this] ~(:kind node-def))
       (~'children [~'this] ~(emit-children-body node-def protocol-name))
       (~'map-children [~'this ~'f] ~(emit-map-children-body node-def))
       (~'node-meta [~'this] (:meta ~'this)))))

(defn- emit-walk-fns [protocol-name]
  `((defn ~'postwalk [f# node#]
      (letfn [(walk# [n#]
                (if (satisfies? ~protocol-name n#)
                  (f# (~'map-children n# walk#))
                  (f# n#)))]
        (walk# node#)))

    (defn ~'prewalk [f# node#]
      (letfn [(walk# [n#]
                (if (satisfies? ~protocol-name n#)
                  (let [n'# (f# n#)]
                    (~'map-children n'# walk#))
                  (f# n#)))]
        (walk# node#)))

    (defn ~'postwalk-env [f# env# node#]
      (letfn [(walk# [env'# n#]
                (if (satisfies? ~protocol-name n#)
                  (let [[new-node# new-env#] (f# (~'map-children n# (partial walk# env'#)) env'#)]
                    [new-node# new-env#])
                  (f# n# env'#)))]
        (walk# env# node#)))

    (defn ~'prewalk-env [f# env# node#]
      (letfn [(walk# [env'# n#]
                (if (satisfies? ~protocol-name n#)
                  (let [[n'# new-env#] (f# n# env'#)]
                    [n'# (~'map-children n'# (partial walk# new-env#))])
                  (f# n# env'#)))]
        (walk# env# node#)))))

(defmacro defast
  "声明式定义 AST 节点系统。

  生成内容：
  1. 协议 ProtocolName：
     - kind       : 节点类型关键字
     - children   : 返回直接子节点向量
     - map-children : 用函数 f 映射所有子节点，返回新节点
     - node-meta  : 返回节点元数据
  2. 每个节点类型的 defrecord：
     - 字段顺序与 DSL 声明完全一致（属性与子节点按声明顺序出现），最后为 meta 字段。
     - 实现上述协议方法（children/map-children 由类型声明自动推导）
  3. 通用遍历函数：
     - postwalk : 后序遍历，子节点递归后应用 f
     - prewalk  : 前序遍历，先应用 f 再递归子节点
  注意：
  - 属性字段用 'name 表示；子节点字段用 :type 或 :type 'field 表示。
  - 类型声明中可以用 / 分隔备选类型（仅作可读标记，不影响逻辑）。
  - 类型声明中 {:many true} 表示该字段包含多个子节点。
  - 所有节点记录自动包含 meta 字段，node-meta 返回 (:meta this)。"
  [name type-map & node-defs]
  (let [type-infos (parse-type-map type-map)
        nodes (parse-all-nodes type-infos node-defs)
        protocol-form (emit-ast-protocol name)
        node-forms (map #(emit-ast-node name %) nodes)
        walk-forms (emit-walk-fns name)]
    `(do ~protocol-form ~@node-forms ~@walk-forms)))
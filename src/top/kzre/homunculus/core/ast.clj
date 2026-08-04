(ns top.kzre.homunculus.core.ast
  "抽象语法树宏工具。
   提供 defast 宏，用声明式 DSL 定义 AST 节点及其协议，
   自动生成：
   - 一个协议（含 kind, children, reduce-children, node-meta 方法）
   - 各节点的 defrecord 实现
   记录的字段顺序与 DSL 中声明的顺序严格一致（属性、子节点交错），末尾追加 meta 字段。"
  (:require [clojure.string :as str]))

(defn- kind->record-name [kind]
  (let [s (name kind)
        parts (str/split s #"-")]
    (when (re-find #"[^a-zA-Z0-9-]" s)
      (throw (ex-info (str "Invalid character in node kind: " kind) {:kind kind})))
    (symbol (apply str (map str/capitalize parts)))))

(defrecord TypeInfo [name subtypes many?])
(defrecord ChildSpec [field many? optional?])
(defrecord NodeDef [kind record-name attrs children-specs field-order])

(defn emit-ast-protocol [name]
  `(defprotocol ~name
     (~'kind       [~'this] "返回节点类型关键字")
     (~'children   [~'this] "返回直接子节点的向量")
     (~'reduce-children [~'this ~'f ~'env] "用 f 折叠子节点，f 接收 [child env] 返回 [new-child new-env]，返回 [new-node final-env]")
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
                ;; 属性字段 'xxx
                (and (seq? elem) (= 'quote (first elem)))
                (let [field-name (second elem)]
                  (recur rst (conj attrs field-name) specs (conj field-order field-name)))

                ;; 子节点字段 :type
                (keyword? elem)
                (let [type-key elem
                      ;; 先取字段名：如果下一个元素是 ('field-name) 则使用，否则用 type-key
                      [field-name rst']
                      (if (and (seq rst) (seq? (first rst)) (= 'quote (ffirst rst)))
                        [(second (first rst)) (rest rst)]
                        [(symbol (name type-key)) rst])
                      ;; 检查字段名之后是否紧跟可选选项 map
                      next-elem (first rst')
                      options   (when (map? next-elem) next-elem)
                      rst''     (if options (rest rst') rst')
                      many?     (get-in type-infos [type-key :many?] false)
                      optional? (boolean (:optional options))]
                  (recur rst'' attrs (conj specs (->ChildSpec field-name many? optional?)) (conj field-order field-name)))

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
  (let [segments (for [{:keys [field many? optional?]} (:children-specs node-def)]
                   (cond
                     optional?
                     `(if ~field [~field] [])
                     many?
                     `(filter #(satisfies? ~protocol-name %) ~field)
                     :else
                     `(when (satisfies? ~protocol-name ~field) [~field])))]
    `(vec (concat ~@segments))))

(defn- emit-reduce-children-body [node-def]
  (let [specs (:children-specs node-def)]
    (if (empty? specs)
      `[~'this ~'env]   ;; 无子节点，直接返回 this 和 env
      (let [env-sym (gensym "env")
            ;; 构建最内层的 assoc 形式，将所有子节点字段更新回 this
            assoc-form `(assoc ~'this
                          ~@(mapcat (fn [s] [(keyword (:field s)) (:field s)]) specs))
            ;; 最内层表达式： [assoced-this env-sym]
            inner-form `[~assoc-form ~env-sym]
            ;; 从后向前包裹 let 绑定，每层处理一个子节点字段
            body
            (reduce
              (fn [inner spec]
                (let [{:keys [field many? optional?]} spec]
                  (cond
                    optional?
                    `(if ~field
                       (let [[~field ~env-sym] (~'f ~field ~env-sym)]
                         ~inner)
                       ~inner)
                    many?
                    `(let [[~field ~env-sym]
                           (reduce
                             (fn [[xs# e#] item#]
                               (let [[new-item# e2#] (~'f item# e#)]
                                 [(conj xs# new-item#) e2#]))
                             [[] ~env-sym]
                             ~field)]
                       ~inner)
                    :else
                    `(let [[~field ~env-sym] (~'f ~field ~env-sym)]
                       ~inner))))
              inner-form
              (reverse specs))]
        `(let [~env-sym ~'env]
           ~body)))))

(defn- emit-ast-node [protocol-name node-def]
  (let [fields (conj (:field-order node-def) 'meta)]
    `(defrecord ~(:record-name node-def) ~fields
       ~protocol-name
       (~'kind [~'this] ~(:kind node-def))
       (~'children [~'this] ~(emit-children-body node-def protocol-name))
       (~'reduce-children [~'this ~'f ~'env]
         ~(emit-reduce-children-body node-def))
       (~'node-meta [~'this] (:meta ~'this)))))

(defmacro defast
  "声明式定义 AST 节点系统。

  生成内容：
  1. 协议 ProtocolName：
     - kind       : 节点类型关键字
     - children   : 返回直接子节点向量
     - reduce-children : 用 f 从左到右折叠子节点，f 接收 [child env] 返回 [new-child new-env]，最终返回 [new-node final-env]
     - node-meta  : 返回节点元数据
  2. 每个节点类型的 defrecord：
     - 字段顺序与 DSL 声明完全一致，最后为 meta 字段。
     - 实现上述协议方法（children/reduce-children 由类型声明自动推导）
  注意：
  - 属性字段用 'name 表示；子节点字段用 :type 或 :type 'field 表示。
  - 类型声明中可以用 / 分隔备选类型（仅作可读标记，不影响逻辑）。
  - 类型声明中 {:many true} 表示该字段包含多个子节点。
  - 所有节点记录自动包含 meta 字段，node-meta 返回 (:meta this)。"
  [name type-map & node-defs]
  (let [type-infos (parse-type-map type-map)
        nodes (parse-all-nodes type-infos node-defs)
        protocol-form (emit-ast-protocol name)
        node-forms (map #(emit-ast-node name %) nodes)]
    `(do ~protocol-form ~@node-forms)))
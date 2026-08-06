(ns top.kzre.homunculus.backend.hlsl.emit
  "ShaderAST → HLSL 源代码发射器。完全使用 T 宏模板化，缩进由 indent 函数统一处理。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.backend.util.naming :refer [cname]]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.backend.util.format :refer [T]]))

;; ── 空环境记录 ──
(defrecord Env [])
(defn make-env [] (->Env))

;; ── 缩进 ──
(def ^:private indent-size 4)

(defn indent
  "为 rows 中每一行添加缩进前缀。"
  [rows]
  (let [prefix (apply str (repeat indent-size \space))]
    (->> (str/split rows #"\n")
         (map #(str prefix %))
         (str/join "\n"))))

;; ── 类型渲染 ──
(defn- render-type [ir-type]
  (cond
    (ty/vec-type? ir-type)
    (let [elem-type (render-type (ty/vec-element-type ir-type))
          size (ty/vec-size ir-type)]
      (T "${elem-type}[${size}]"))
    (ty/type-sym ir-type) (name (ty/type-sym ir-type))
    :else (throw (ex-info (str "Unknown type: " ir-type) {}))))

;; ── 多方法 ──
(defmulti emit-node (fn [node _env] (ast/kind node)))

;; ── 字面量 ──
(defmethod emit-node :literal [node _]
  (let [val (:val node)]
    (cond
      (nil? val)   nil
      (integer? val) (str val)
      (float? val)   (str val)
      (true? val)    "true"
      (false? val)   "false"
      :else (pr-str val))))

;; ── 变量引用 ──
(defmethod emit-node :variable [node _]
  (cname (name (:name node))))

;; ── 函数调用 ──
(defmethod emit-node :call [node env]
  (let [fn-sym (:fn node)
        args (:args node)]
    (if (= fn-sym 'sample)
      ;; 特判 sample 函数：转换为 texture.Sample(sampler, uv, ...)
      (let [texture (emit-node (first args) env)
            sampler (emit-node (second args) env)
            rest-args (str/join ", " (map #(emit-node % env) (drop 2 args)))]
        (str texture ".Sample(" sampler ", " rest-args ")"))
      ;; 普通函数调用
      (let [fn-name (name fn-sym)
            args-str (str/join ", " (map #(emit-node % env) args))]
        (T "${fn-name}(${args-str})")))))

;; ── 二元运算 ──
(defmethod emit-node :binary-op [node env]
  (let [left (emit-node (:left node) env)
        op (name (:op node))
        right (emit-node (:right node) env)]
    (T "${left} ${op} ${right}")))

;; ── 一元运算 ──
(defmethod emit-node :unary-op [node env]
  (let [op (name (:op node))
        expr (emit-node (:expr node) env)]
    (T "${op}${expr}")))

;; ── 成员访问 ──
(defmethod emit-node :member-access [node env]
  (let [target (emit-node (:target node) env)
        member (name (:member node))]
    (T "${target}.${member}")))

;; ── 数组索引 ──
(defmethod emit-node :array-index [node env]
  (let [target (emit-node (:target node) env)
        index (emit-node (:index node) env)]
    (T "${target}[${index}]")))

;; ── 构造器 ──
(defmethod emit-node :constructor [node env]
  (let [ty (:type node)
        vec? (ty/vec-type? ty)
        struct? (:struct? (:meta node))
        type-str (when-not (or vec? struct?) (render-type ty))
        args (str/join ", " (map #(emit-node % env) (:args node)))]
    (cond
      (or vec? struct?) (T "{${args}}")
      :else             (T "${type-str}(${args})"))))


;; ── 类型转换 ──
(defmethod emit-node :cast [node env]
  (let [type (render-type (:type node))
        expr (emit-node (:expr node) env)]
    (T "(${type})${expr}")))

(defn- render-var-decl [ir-type var-name]
  (if (ty/vec-type? ir-type)
    (let [elem (render-type (ty/vec-element-type ir-type))
          size (ty/vec-size ir-type)]
      (str elem " " var-name "[" size "]"))
    (str (render-type ir-type) " " var-name)))

;; ── 变量声明 ──
(defmethod emit-node :var-decl [node env]
  (let [ty (:type node)
        name (cname (name (:name node)))
        init (:init node)
        init-str (when init (emit-node init env))]
    (if (str/blank? init-str)
      (render-var-decl ty name)
      (let [init-str (emit-node init env)]
        (str (render-var-decl ty name) " = " init-str)))))

(defmethod emit-node :uniform [node env]
  (let [ty (:type node)
        name (cname (name (:name node)))]
    (str "uniform " (render-var-decl ty name) ";")))

(defmethod emit-node :static-var [node env]
  (let [ty (:type node)
        name (cname (name (:name node)))
        init (:init node)
        init-str (when init (emit-node init env))]
    (if (str/blank? init-str)
      (str "static " (render-var-decl ty name) ";")
      (str "static " (render-var-decl ty name) " = " (emit-node init env) ";"))))

;; ── 赋值 ──
(defmethod emit-node :assign [node env]
  (let [lhs (emit-node (:lhs node) env)
        rhs (emit-node (:rhs node) env)]
    (T "${lhs} = ${rhs}")))

;; ── 语句结尾分号 ──
(defn- stmt-needs-semicolon? [node]
  (not (contains? #{:if :while :block :function :entry-point
                    :struct :resource-decl :import}
                  (ast/kind node))))

(defn- pure-value-node?
  "判断节点是否为纯值表达式，即不能独立作为语句的节点。"
  [node]
  (contains? #{:variable :literal :member-access :array-index} (ast/kind node)))

;; ── 块体渲染 ──
(defn- emit-block-body [block env top-level?]
  (let [stmts (:stmts block)
        ret   (:ret block)
        ;; 过滤掉无意义的纯值语句
        lines (keep (fn [s]
                      (when-not (pure-value-node? s)
                        (let [code (emit-node s env)]
                          (if (stmt-needs-semicolon? s)
                            (str code ";")
                            code))))
                    stmts)
        body-str (str/join "\n" lines)
        body-str (if (str/blank? body-str) "" (str body-str "\n"))]
    (if (and top-level? ret)
      (let [ret-str (emit-node ret env)]
        (str body-str "return " ret-str ";"))
      body-str)))

;; ── if 语句 ──
(defmethod emit-node :if [node env]
  (let [test-str (emit-node (:test node) env)
        then-str (indent (emit-block-body (:then node) env false))
        else-str (when-let [e (:else node)]
                   (indent (emit-block-body e env false)))]
    (if else-str
      (T "if (${test-str})\n{\n${then-str}\n}\nelse\n{\n${else-str}\n}")
      (T "if (${test-str})\n{\n${then-str}\n}"))))

;; ── while 语句 ──
(defmethod emit-node :while [node env]
  (let [test-str (emit-node (:test node) env)
        body-str (indent (emit-block-body (:body node) env false))]
    (T "while (${test-str})\n{\n${body-str}\n}")))

;; ── block ──
(defmethod emit-node :block [node env]
  (emit-block-body node env false))

;; ── 普通函数 ──
(defmethod emit-node :function [node env]
  (let [name-str (cname (name (:name node)))
        ret-type (render-type (:return-type node))
        params (:params node)
        param-str (str/join ", " (map #(let [type (render-type (:type %))
                                             name (cname (name (:name %)))]
                                         (T "${type} ${name}"))
                                      params))
        body-str (indent (emit-block-body (:body node) env true))]
    (T "${ret-type} ${name-str}(${param-str})\n{\n${body-str}\n}")))

;; ── 入口点 ──
(defmethod emit-node :entry-point [node env]
  ((get-method emit-node :function) node env))

;; ── 结构体 ──
(defmethod emit-node :struct [node env]
  (let [name-str (name (:name node))
        members (:members node)
        member-str (when (seq members)
                     (indent (str/join "\n" (map #(let [type (render-type (:type %))
                                                        name (name (:name %))]
                                                    (T "${type} ${name};"))
                                                 members))))]
    (T "struct ${name-str}\n{\n${member-str}\n};")))

;; ── 资源声明 ──
(defmethod emit-node :resource-decl [node env]
  (let [res-name (name (:name node))
        res-slot (name (:slot node))
        kind (:resource-kind node)]
    (case kind
      :texture2D (T "Texture2D ${res-name} : register(${res-slot});")
      :sampler   (T "SamplerState ${res-name} : register(${res-slot});")
      :cbuffer   (let [members (:members node)
                       member-str (when (seq members)
                                    (indent (str/join "\n" (map #(let [type (render-type (:type %))
                                                                       mem-name (name (:name %))]
                                                                   (T "${type} ${mem-name};"))
                                                                members))))]
                   (T "cbuffer ${res-name} : register(${res-slot})\n{\n${member-str}\n}")))))

;; ── import ──
(defmethod emit-node :import [node _]
  (let [path (str (:path node))]
    (T "#include \"${path}.hlsl\"")))

;; ── 默认报错 ──
(defmethod emit-node :default [node _]
  (throw (ex-info (str "Unknown ShaderAST node: " (ast/kind node)) {:node node})))

;; ── 入口 ──
(defn emit-nodes [nodes]
  (let [env (make-env)
        filtered (remove #(and (= :var-decl (ast/kind %))
                               (-> % :meta :shader/ignore-emit?))
                         nodes)]
    (str
      (str/join "\n\n" (map #(emit-node % env) filtered))
      "\n")))
(ns top.kzre.homunculus.backend.hlsl.emit
  "ShaderAST → HLSL 源代码发射器。完全使用 T 宏模板化，缩进由 indent 函数统一处理。"
  (:require
   [clojure.string :as str]
   [top.kzre.homunculus.backend.hlsl.env :as env]
   [top.kzre.homunculus.backend.hlsl.lang :as lang]
   [top.kzre.homunculus.backend.shader.ast :as ast]
   [top.kzre.homunculus.backend.shader.semantic :as semantic]
   [top.kzre.homunculus.backend.util.format :refer [T]]
   [top.kzre.homunculus.backend.util.naming :refer [cname]]
   [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ── 缩进 ──
(def ^:private indent-size 4)

(defn indent
  "为 rows 中每一行添加缩进前缀。"
  [rows]
  (let [prefix (apply str (repeat indent-size \space))]
    (->> (str/split rows #"\n")
         (map #(str prefix %))
         (str/join "\n"))))


;; ── 多方法 ──
(defmulti emit-node* (fn [node _env] (ast/kind node)))

;; ── 函数调用 ──
(defmethod emit-node* :call [node env]
  (let [fn-sym (:fn node)
        args (:args node)]
    (if (= fn-sym 'sample)
      ;; 特判 sample 函数：转换为 texture.Sample(sampler, uv, ...)
      (let [texture (emit-node* (first args) env)
            sampler (emit-node* (second args) env)
            rest-args (str/join ", " (map #(emit-node* % env) (drop 2 args)))]
        (str texture ".Sample(" sampler ", " rest-args ")"))
      ;; 普通函数调用
      (let [fn-name (name fn-sym)
            args-str (str/join ", " (map #(emit-node* % env) args))]
        (T "${fn-name}(${args-str})")))))

;; ── 构造器 ──
(defmethod emit-node* :constructor [node env]
  (let [ty (:type node)
        vec? (ty/vec-type? ty)
        struct? (:struct? (:meta node))
        type-str (when-not (or vec? struct?) (lang/type->str ty))
        args (str/join ", " (map #(emit-node* % env) (:args node)))]
    (cond
      (or vec? struct?) (T "{${args}}")
      :else             (T "${type-str}(${args})"))))


(defn- render-var-decl [ir-type var-name]
  (if (ty/vec-type? ir-type)
    (let [elem (lang/type->str (ty/vec-element-type ir-type))
          size (ty/value-val (ty/vec-size ir-type))]
      (str elem " " var-name "[" size "]"))
    (str (lang/type->str ir-type) " " var-name)))

;; ── 变量声明 ──
(defmethod emit-node* :var-decl [node env]
  (let [ty (:type node)
        name (cname (name (:name node)))
        init (:init node)
        init-str (when init (emit-node* init env))]
    (if (str/blank? init-str)
      (render-var-decl ty name)
      (let [init-str (emit-node* init env)]
        (str (render-var-decl ty name) " = " init-str)))))


(defmethod emit-node* :static-var [node env]
  (let [ty (:type node)
        name (cname (name (:name node)))
        init (:init node)
        init-str (when init (emit-node* init env))]
    (if (str/blank? init-str)
      (str "static " (render-var-decl ty name) ";")
      (str "static " (render-var-decl ty name) " = " (emit-node* init env) ";"))))


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
                        (let [code (emit-node* s env)]
                          (if (stmt-needs-semicolon? s)
                            (str code ";")
                            code))))
                    stmts)
        body-str (str/join "\n" lines)
        body-str (if (str/blank? body-str) "" (str body-str "\n"))]
    (if (and top-level? ret)
      (let [ret-str (emit-node* ret env)]
        (str body-str "return " ret-str ";"))
      body-str)))

;; ── if 语句 ──
(defmethod emit-node* :if [node env]
  (let [test-str (emit-node* (:test node) env)
        then-str (indent (emit-block-body (:then node) env false))
        else-str (when-let [e (:else node)]
                   (indent (emit-block-body e env false)))]
    (if else-str
      (T "if (${test-str})\n{\n${then-str}\n}\nelse\n{\n${else-str}\n}")
      (T "if (${test-str})\n{\n${then-str}\n}"))))


;; ── 普通函数 ──
(defmethod emit-node* :function [node env]
  (let [name-str (cname (name (:name node)))
        ret-type (lang/type->str (:return-type node))
        params (:params node)
        param-str (if (seq params)                          ;; 函数参数可能为空
                    (str/join ", " (map #(let [type (lang/type->str (:type %))
                                               name (cname (name (:name %)))]
                                           (T "${type} ${name}"))
                                        params))
                    "")
        body-str (indent (emit-block-body (:body node) env true))]
    (T "${ret-type} ${name-str}(${param-str})\n{\n${body-str}\n}")))


;; ── 入口点 ──
(defmethod emit-node* :entry-point [node env]
  (let [name-str (cname (name (:name node)))
        ret-type (lang/type->str (:return-type node))
        params (:params node)
        stage (:stage node)
        sematic (semantic/shader-semantic node)
        param-str (str/join ", " (map #(let [type (lang/type->str (:type %))
                                             name (cname (name (:name %)))
                                             sematic (semantic/shader-semantic %)]
                                         (if sematic
                                           (let [s (lang/sematic->str sematic stage)]
                                             (T "${type} ${name} : ${s}"))
                                           (T "${type} ${name}")))
                                      params))

        body-str (indent (emit-block-body (:body node) env true))]
    (if sematic
      ;; 输出用于片元阶段
      (let [s (lang/sematic->str sematic :fragment)]
        (T "${ret-type} ${name-str}(${param-str}) : ${s}\n{\n${body-str}\n}"))
      (T "${ret-type} ${name-str}(${param-str})\n{\n${body-str}\n}"))))

;; ── 结构体 ──
(defmethod emit-node* :struct [node env]
  (let [name-str (name (:name node))
        members (:members node)
        member-str (when (seq members)
                     (indent (str/join
                               "\n"
                               (map #(let [type (lang/type->str (:type %))
                                           name (name (:name %))
                                           sematic (semantic/shader-semantic %)]
                                       (if sematic
                                         (let [s (lang/sematic->str sematic :vertex)]
                                           (T "${type} ${name} : ${s};"))
                                         (T "${type} ${name};")))
                                    members))))]
    (T "struct ${name-str}\n{\n${member-str}\n};")))

;; ── 资源声明 ──
(defmethod emit-node* :resource-decl [node env]
  (let [res-name (name (:name node))
        res-slot (name (:slot node))
        kind (:resource-kind node)]
    (case kind
      :texture   (T "Texture2D ${res-name} : register(${res-slot});")
      :sampler   (T "SamplerState ${res-name} : register(${res-slot});")
      :cbuffer   (let [members (:members node)
                       member-str (when (seq members)
                                    (indent (str/join "\n" (map #(let [type (lang/type->str (:type %))
                                                                       mem-name (name (:name %))]
                                                                   (T "${type} ${mem-name};"))
                                                                members))))]
                   (T "cbuffer ${res-name} : register(${res-slot})\n{\n${member-str}\n}")))))

(defmethod emit-node* :default [node env]
  (case (ast/kind node)
    :literal
    (let [val (:val node)]
      (lang/literal->str val))
    :import
    (let [path (str (:path node))]
      (T "#include \"${path}.hlsl\""))
    :variable
    (cname (name (:name node)))
    :binary-op
    (let [left (emit-node* (:left node) env)
          op (name (:op node))
          right (emit-node* (:right node) env)]
      (T "${left} ${op} ${right}"))
    :unary-op
    (let [op (name (:op node))
          expr (emit-node* (:expr node) env)]
      (T "${op}${expr}"))
    :block
    (emit-block-body node env false)
    :while
    (let [test-str (emit-node* (:test node) env)
          body-str (indent (emit-block-body (:body node) env false))]
      (T "while (${test-str})\n{\n${body-str}\n}"))
    :assign
    (let [lhs (emit-node* (:lhs node) env)
          rhs (emit-node* (:rhs node) env)]
      (T "${lhs} = ${rhs}"))
    :cast
    (let [type (lang/type->str (:type node))
          expr (emit-node* (:expr node) env)]
      (T "${type}(${expr})"))
    :array-index
    (let [target (emit-node* (:target node) env)
          index (emit-node* (:index node) env)]
      (T "${target}[${index}]"))
    :uniform
    (let [ty (:type node)
          name (cname (name (:name node)))]
      (str "uniform " (render-var-decl ty name) ";"))
    :member-access
    (let [target (emit-node* (:target node) env)
          member (name (:member node))]
      (T "${target}.${member}"))
    (throw (ex-info (str "Unknown ShaderAST node: " (ast/kind node)) {:node node}))))

;; ── 入口 ──
(defn emit [nodes]
  (let [env (env/make-env)
        filtered (remove #(and (= :var-decl (ast/kind %))
                               (-> % :meta :shader/ignore-emit?))
                         nodes)]
    (str
      (str/join "\n\n" (map #(emit-node* % env) filtered))
      "\n")))
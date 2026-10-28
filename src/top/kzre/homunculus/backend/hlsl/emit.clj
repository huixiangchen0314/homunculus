(ns top.kzre.homunculus.backend.hlsl.emit
  "ShaderAST → HLSL 源代码发射器。完全使用 T 宏模板化，缩进由 indent 函数统一处理。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.clang.format :as clang.format]
    [top.kzre.homunculus.backend.clang.lang :as clang.lang]
    [top.kzre.homunculus.backend.format :as format :refer [T]]
    [top.kzre.homunculus.backend.hlsl.env :as env]
    [top.kzre.homunculus.backend.hlsl.format :as hlsl.format]
    [top.kzre.homunculus.backend.hlsl.lang :as lang]
    [top.kzre.homunculus.backend.indent :as indent]
    [top.kzre.homunculus.frontend.shader.ast :as ast]
    [top.kzre.homunculus.frontend.shader.semantic :as semantic]
    [top.kzre.homunculus.core.error :as err]
    [top.kzre.homunculus.core.ir2.pass.type :as ty]))

;; ── 多方法 ──
(defmulti emit-node* (fn [node _env] (ast/kind node)))

(defn- emit-node [node env]
  (emit-node* node env))

(defn- binary-child-op
  [node]
  (when (= :binary-op (ast/kind node))
    (:op node)))

(defmethod emit-node* :binary-op [node env]
  (let [op    (clang.lang/naming-operator (:op node))
        left  (:left node)
        right (:right node)]
    (clang.format/binary-op
      op
      (emit-node left env)
      (emit-node right env)
      :left-parens?  (clang.lang/operand-needs-parens?
                       (binary-child-op left) op false)
      :right-parens? (clang.lang/operand-needs-parens?
                       (binary-child-op right) op true))))

(defmethod emit-node* :unary-op [node env]
  (let [child (:expr node)]
    (clang.format/unary-op
      (clang.lang/naming-operator (:op node))
      (emit-node child env)
      :operand-parens? (clang.lang/unary-operand-needs-parens?
                         (binary-child-op child)))))

(def ^:private texture-method-names
  "纹理方法调用名映射。

   u/i 前缀的变体在 HLSL 里映射到同样的方法名——HLSL 的 Texture2D<T> /
   Buffer<T> 通过模板参数携带 texel 类型，方法名本身不需要区分。
   `usample` 和 `sample` 都是 `.Sample(...)`，返回类型由 texture 的
   T 决定（Texture2D<uint4> 上 Sample 返回 uint4）。

   差异由 texture-decl 的模板参数承载，不在方法名上。"
  {;; ── float 变体 ──
   'sample              "Sample"
   'sampleLod           "SampleLevel"
   'sampleBias          "SampleBias"
   'sampleCmp           "SampleCmp"
   'sampleCmpLevelZero  "SampleCmpLevelZero"
   'sampleGrad          "SampleGrad"
   'gather              "Gather"
   'gatherRed           "GatherRed"
   'gatherGreen         "GatherGreen"
   'gatherBlue          "GatherBlue"
   'gatherAlpha         "GatherAlpha"
   'load                "Load"
   'textureSize         "GetDimensions"

   ;; ── uint 变体 ──
   'usample             "Sample"
   'usampleLod          "SampleLevel"
   'usampleBias         "SampleBias"
   'usampleGrad         "SampleGrad"
   'ugather             "Gather"
   'ugatherRed          "GatherRed"
   'ugatherGreen        "GatherGreen"
   'ugatherBlue         "GatherBlue"
   'ugatherAlpha        "GatherAlpha"
   'uload               "Load"

   ;; ── int 变体 ──
   'isample             "Sample"
   'isampleLod          "SampleLevel"
   'isampleBias         "SampleBias"
   'isampleGrad         "SampleGrad"
   'igather             "Gather"
   'igatherRed          "GatherRed"
   'igatherGreen        "GatherGreen"
   'igatherBlue         "GatherBlue"
   'igatherAlpha        "GatherAlpha"
   'iload               "Load"})

(def ^:private component-compare-ops
  {'lessThan         "<"
   'lessThanEqual    "<="
   'greaterThan      ">"
   'greaterThanEqual ">="
   'equal            "=="
   'notEqual         "!="})

;; ── 函数调用 ──
(defmethod emit-node* :call [node env]
  (let [fn-sym   (:fn node)
        args     (:args node)
        arg-strs (mapv #(emit-node* % env) args)]
    (cond
      (contains? texture-method-names fn-sym)
      (let [tex      (first arg-strs)
            rest-str (str/join ", " (rest arg-strs))]
        (str tex "." (texture-method-names fn-sym) "(" rest-str ")"))

      (contains? component-compare-ops fn-sym)
      (do
        (when (not= 2 (count arg-strs))
          (throw (err/compile-error
                   :arity-mismatch
                   (str fn-sym " expects exactly 2 arguments")
                   {:node node :arg-count (count arg-strs)})))
        (let [left  (first arg-strs)
              right (second arg-strs)
              op    (component-compare-ops fn-sym)]
          (T "(${left} ${op} ${right})")))

      :else
      (clang.format/call fn-sym arg-strs))))

;; ── 构造器 ──
(defmethod emit-node* :constructor [node env]
  (let [ty (:type node)
        vec? (ty/vec-type? ty)
        struct? (:struct? (:meta node))]
    (if (or vec? struct?)
      (clang.format/array-construct (map #(emit-node* % env) (:args node)))
      (clang.format/construct
        (clang.lang/type->str ty)
        (map #(emit-node* % env) (:args node))))))

(defn- render-var-decl [ir-type var-name]
  (let [type-str (clang.lang/type->str ir-type)]
    (clang.format/var-decl (clang.lang/naming var-name) type-str)))

;; ── 变量声明 ──
(defmethod emit-node* :var-decl [node env]
  (let [ty        (:type node)
        init-node (:init node)
        init-str  (when (some? init-node)
                    (let [s (emit-node* init-node env)]
                      (when-not (str/blank? s)
                        (str " = " s))))
        type-str  (clang.lang/type->str ty)]
    (str (clang.format/var-decl (clang.lang/naming (:name node)) type-str) init-str)))

(defmethod emit-node* :static-var [node env]
  (let [ty        (:type node)
        init-node (:init node)
        init-str  (when (some? init-node)
                    (let [s (emit-node* init-node env)]
                      (when-not (str/blank? s)
                        (str " = " s))))
        type-str  (clang.lang/type->str ty)]
    (str (clang.format/var-decl (clang.lang/naming (:name node)) type-str :static? true)
         init-str ";")))

;; ── 语句结尾分号 ──
(defn- stmt-needs-semicolon? [node]
  (not (contains? #{:if :while :block :function :entry-point
                    :struct :import}
                  (ast/kind node))))

(defn- pure-value-node?
  [node]
  (contains? #{:variable :literal :member-access :array-index} (ast/kind node)))

;; ── 块体渲染 ──
(defn- emit-block-body [block env top-level?]
  (let [stmts (:stmts block)
        ret   (:ret block)
        lines (keep (fn [s]
                      (when-not (pure-value-node? s)
                        (let [code (emit-node* s env)]
                          (if (stmt-needs-semicolon? s)
                            (str code ";")
                            code))))
                    stmts)
        body-str (format/rows lines)]
    (if (and top-level? ret)
      (let [ret-str  (emit-node* ret env)
            ret-line (str "return " ret-str ";")]
        (if (str/blank? body-str)
          ret-line
          (str body-str "\n" ret-line)))
      body-str)))

;; ── if 语句 ──
(defmethod emit-node* :if [node env]
  (clang.format/if
    :test (emit-node* (:test node) env)
          :then (indent/indent (emit-block-body (:then node) env false))
          :else (when-let [e (:else node)]
                  (indent/indent (emit-block-body e env false)))))

;; ── 普通函数 ──
(defmethod emit-node* :function [node env]
  (let [ret-type (clang.lang/type->str (:return-type node))
        params   (mapv (fn [p]
                         (clang.format/param
                           :type (clang.lang/type->str (:type p))
                           :name (clang.lang/naming (:name p))))
                       (:params node))
        body     (indent/indent (emit-block-body (:body node) env true))]
    (clang.format/function
      :name (clang.lang/naming  (:name node))
      :return-type ret-type
      :params params
      :body body)))

;; ── 入口点 ──
(defmethod emit-node* :entry-point [node env]
  (let [ret-type (clang.lang/type->str (:return-type node))
        stage    (:stage node)
        sematic  (semantic/shader-semantic node)
        after    (when sematic
                   (str ": " (lang/sematic->str sematic stage)))
        params   (map (fn [p]
                        (let [sem (semantic/shader-semantic p)]
                          (clang.format/param
                            :type (clang.lang/type->str (:type p))
                            :name (clang.lang/naming (:name p))
                            :after (when sem
                                     (str ": " (lang/sematic->str sem stage))))))
                      (:params node))
        body     (indent/indent (emit-block-body (:body node) env true))]
    (clang.format/function
      :name (clang.lang/naming (:name node))
      :return-type ret-type
      :params params
      :body body
      :after after)))

;; ── 结构体 ──
(defmethod emit-node* :struct [node env]
  (let [member-strs (mapv (fn [m]
                            (let [type (clang.lang/type->str (:type m))
                                  sem  (semantic/shader-semantic m)]
                              (if sem
                                (clang.format/struct-member
                                  :name (clang.lang/naming (:name m)) :type type
                                  :after (str ": " (lang/sematic->str sem :vertex)))
                                (clang.format/struct-member
                                  :name (clang.lang/naming (:name m))
                                  :type type))))
                          (:members node))
        members     (indent/indent (format/rows member-strs))]
    (clang.format/struct
      :name (clang.lang/naming (:name node))
      :members members)))

;; ── 资源声明 ──

(defmethod emit-node* :texture-decl [node env]
  (let [res-name  (name (:name node))
        res-reg   (name (:register node))
        dim       (:dimension node)
        tex-type  (name (lang/texture-dimension->type-sym dim))
        elem-type (clang.lang/type->str (:element-type node))]
    (T "${tex-type}<${elem-type}> ${res-name} : register(${res-reg});")))

(defmethod emit-node* :sampler-decl [node env]
  (let [res-name (name (:name node))
        res-reg  (name (:register node))
        cmp?     (:compare? node)
        type-str (or (lang/sampler-type-str cmp?)
                     (throw (err/compile-error
                              :unknown-sampler-compare
                              (str "Unknown sampler compare?: " cmp?))))]
    (T "${type-str} ${res-name} : register(${res-reg});")))

(defmethod emit-node* :cbuffer-decl [node env]
  (let [res-name   (name (:name node))
        res-reg    (name (:register node))
        members    (:members node)
        member-str (when (seq members)
                     (indent/indent
                       (str/join "\n"
                                 (map #(let [t (clang.lang/type->str (:type %))
                                             n (name (:name %))]
                                         (T "${t} ${n};"))
                                      members))))]
    (T "cbuffer ${res-name} : register(${res-reg})\n{\n${member-str}\n}")))

(def ^:private access-projected-target
  #{:binary-op :unary-op :array-index
    :member-access :constructor :cast :assign})
(defmethod emit-node* :member-access [node env]
  (let [target-node (:target node)
        protected? (contains? access-projected-target (ast/kind target-node))]
    (clang.format/member-access
      (if protected?
        (str "(" (emit-node target-node env) ")")
        (emit-node target-node env))
      (clang.lang/naming (:member node)))))

(defmethod emit-node* :default [node env]
  (case (ast/kind node)
    :literal
    (if (some? (:val node))
      (clang.lang/literal->str (:val node))
      "")
    :import
    (let [path (str (:path node) ".hlsl")]
      (clang.format/include path))
    :variable
    (clang.lang/naming (:name node))
    :block
    (emit-block-body node env false)
    :while
    (clang.format/while
      (emit-node* (:test node) env)
      (indent/indent (emit-block-body (:body node) env false)))
    :assign
    (clang.format/assign
      (emit-node* (:lhs node) env)
      (emit-node* (:rhs node) env))
    :cast
    (hlsl.format/cast
      (:type node)
      (emit-node* (:expr node) env))
    :array-index
    (clang.format/array-index
      (emit-node* (:target node) env)
      (emit-node* (:index node) env))
    :uniform
    (let [ty   (:type node)]
      (str "uniform " (render-var-decl ty (:name node)) ";"))
    (throw (ex-info (str "Unknown ShaderAST node: " (ast/kind node)) {:node node}))))

;; ── 入口 ──
(defn emit [nodes]
  (let [env      (env/make-env)
        filtered (remove #(and (= :var-decl (ast/kind %))
                               (semantic/shader-ignore-emit? %))
                         nodes)]
    (str
      (str/join "\n\n" (map #(emit-node % env) filtered))
      "\n")))
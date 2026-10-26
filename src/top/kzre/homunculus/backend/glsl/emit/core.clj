(ns top.kzre.homunculus.backend.glsl.emit.core
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.clang.format :as clang.format]
    [top.kzre.homunculus.backend.clang.lang :as clang.lang]
    [top.kzre.homunculus.backend.format :as format :refer [T]]
    [top.kzre.homunculus.backend.glsl.ast :as ast]
    [top.kzre.homunculus.backend.glsl.emit.env :as env]
    [top.kzre.homunculus.backend.glsl.lang :as lang]
    [top.kzre.homunculus.backend.hlsl.format :as hlsl.format]
    [top.kzre.homunculus.backend.indent :as indent]
    [top.kzre.homunculus.backend.shader.semantic :as semantic])
  (:import
    (top.kzre.homunculus.backend.shader.pass.analyze.model SLAnalysis)))

(defn- stmt-needs-semicolon?
  "stmt 是否需要追加分号。

   :interface-block / :struct / :function / :uniform-block
   等顶层声明自身带分号或不需要分号——不在此集合。
   :interface-block 已经在 emit-node* 内自带 `};`，
   若再加入会在 emit-block-body 处变成 `};;`。"
  [node]
  (contains? #{:assign :var-decl :uniform :global-var
               :input-decl :output-decl}
             (ast/kind node)))

;; ═══════════════════════════════════════════
;; 多方法分派
;; ═══════════════════════════════════════════

(declare emit-node)

(defmulti emit-node*
          (fn [node _env] (ast/kind node)))

(defn- emit-node [node env]
  (emit-node* node env))

;; ═══════════════════════════════════════════
;; 表达式
;; ═══════════════════════════════════════════

(defmethod emit-node* :literal [node _]
  (clang.lang/literal->str (:val node)))

(defmethod emit-node* :variable [node _]
  (lang/naming (:name node)))

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

(defmethod emit-node* :call [node env]
  (clang.format/call
    (lang/naming-fn (:fn node))
    (map #(emit-node % env) (:args node))))

(defmethod emit-node* :member-access [node env]
  (clang.format/member-access
    (emit-node (:target node) env)
    (lang/naming (:member node))))

(defmethod emit-node* :array-index [node env]
  (clang.format/array-index
    (emit-node (:target node) env)
    (emit-node (:index node) env)))

(defmethod emit-node* :constructor [node env]
  (let [type-str (lang/naming-fn (:type node))
        args     (str/join ", " (map #(emit-node % env) (:args node)))]
    (T "${type-str}(${args})")))

(defmethod emit-node* :cast [node env]
  (hlsl.format/cast
    (lang/naming-fn (:type node))
    (emit-node (:expr node) env)))

;; ═══════════════════════════════════════════
;; 语句
;; ═══════════════════════════════════════════

(defmethod emit-node* :assign [node env]
  (clang.format/assign
    (emit-node (:lhs node) env)
    (emit-node (:rhs node) env)))

(defmethod emit-node* :var-decl [node env]
  (hlsl.format/var-decl
    :name (lang/naming (:name node))
    :type (:type node)
    :init (when-let [i (:init node)]
            (emit-node i env))))

(defmethod emit-node* :uniform [node _]
  (let [decl (clang.format/var-decl
               (:name node)
               (:type node))]
    (T "uniform ${decl};")))

(defmethod emit-node* :global-var [node env]
  (let [type-str (:type node)
        name     (:name node)
        const    (if (:const? node) "const " "")
        init     (when-let [i (:init node)]
                   (emit-node i env))]
    (if init
      (T "${const}${type-str} ${name} = ${init};")
      (T "${const}${type-str} ${name};"))))

(defmethod emit-node* :input-decl [node _]
  (let [type-str (:type node)
        name     (:name node)
        loc      (when-let [l (:location node)]
                   (str "layout(location = " l ") "))
        flat     (if (:flat? node) "flat " "")]
    (T "${loc}${flat}in ${type-str} ${name};")))

(defmethod emit-node* :output-decl [node _]
  (let [type-str (:type node)
        name     (:name node)
        loc      (when-let [l (:location node)]
                   (str "layout(location = " l ") "))
        flat     (if (:flat? node) "flat " "")]
    (T "${loc}${flat}out ${type-str} ${name};")))

;; ═══════════════════════════════════════════
;; 控制流
;; ═══════════════════════════════════════════

(defn- emit-block-body
  [block env]
  (let [stmts   (keep (fn [s]
                        (let [code (emit-node s env)]
                          (cond
                            (str/blank? code) nil
                            (stmt-needs-semicolon? s) (str code ";")
                            :else code)))
                      (:stmts block))
        ret-str (when-let [r (:ret block)]
                  (let [ret-value (emit-node r env)]
                    (str "return " ret-value ";")))]
    (format/rows (concat stmts (when ret-str [ret-str])))))

(defmethod emit-node* :if [node env]
  (clang.format/if
    :test (emit-node (:test node) env)
          :then (indent/indent (emit-block-body (:then node) env))
          :else (when-let [e (:else node)]
                  (indent/indent (emit-block-body e env)))))

(defmethod emit-node* :while [node env]
  (let [body (:body node)
        body-str (if (= :block (ast/kind body))
                   (indent/indent (emit-block-body body env))
                   (indent/indent (emit-node body env)))]
    (clang.format/while (emit-node (:test node) env) body-str)))

(defmethod emit-node* :for [node env]
  (clang.format/loopfor
    :init   (when-let [i (:init node)] (emit-node i env))
    :test   (emit-node (:test node) env)
    :update (when-let [u (:update node)] (emit-node u env))
    :body   (indent/indent (emit-block-body (:body node) env))))

(defmethod emit-node* :block [node env]
  (clang.format/block (indent/indent (emit-block-body node env))))

;; ═══════════════════════════════════════════
;; 顶层声明
;; ═══════════════════════════════════════════

(defmethod emit-node* :param [node _]
  (clang.format/param
    :type (:type node)
    :name (lang/naming (:name node))))

(defmethod emit-node* :struct-member [node _]
  (clang.format/struct-member
    :name (lang/naming (:name node))
    :type (:type node)))

(defmethod emit-node* :interface-block-member [node _]
  (let [type-str (:type node)
        name     (:name node)
        loc      (when-let [l (:location node)]
                   (str "layout(location = " l ") "))
        flat     (if (:flat? node) "flat " "")]
    (T "${loc}${flat}${type-str} ${name};")))

(defmethod emit-node* :struct [node env]
  (clang.format/struct
    :name (:name node)
    :members (-> (map #(emit-node % env) (:members node))
                 (format/rows)
                 (indent/indent))))

(defmethod emit-node* :uniform-block [node env]
  (let [name         (:name node)
        layout-parts (cond-> []
                             (:packing node) (conj (lang/naming (:packing node)))
                             (:binding node) (conj (str "binding = " (:binding node))))
        layout-str   (if (seq layout-parts)
                       (let [joined (str/join ", " layout-parts)]
                         (str "layout(" joined ") "))
                       "")
        members      (->> (:members node)
                          (map #(emit-node % env))
                          (str/join "\n")
                          (indent/indent))]
    (T "${layout-str}uniform ${name} {\n${members}\n};")))

(defmethod emit-node* :interface-block [node env]
  (let [dir      (:direction node)
        dir-str  (case dir
                   :in  "in"
                   :out "out"
                   (throw (ex-info "interface-block missing :direction"
                                   {:name (:name node) :direction dir})))
        name     (:name node)
        instance (:instance-name node)
        loc      (when-let [l (:location node)]
                   (str "layout(location = " l ") "))
        members  (->> (:members node)
                      (map #(emit-node % env))
                      (format/rows)
                      (indent/indent))]
    (T "${loc}${dir-str} ${name} {\n${members}\n} ${instance};")))

(defmethod emit-node* :function [node env]
  (clang.format/function
    :name (lang/naming (:name node))
    :return-type (:return-type node)
    :params (map #(emit-node % env) (:params node))
    :body (indent/indent (emit-block-body (:body node) env))))

;; ═══════════════════════════════════════════
;; 默认
;; ═══════════════════════════════════════════

(defmethod emit-node* :default [node _]
  (str "/* UNIMPLEMENTED: " (pr-str node) " */"))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn- emit-version [env]
  (let [version (env/gl-version env)]
    (T "#version ${version} core")))

(defn emit
  [nodes env ^SLAnalysis _analysis]
  (let [header (emit-version env)
        body   (->> nodes
                    (remove semantic/shader-ignore-emit?)
                    (keep #(emit-node % env))
                    (remove str/blank?)
                    (str/join "\n\n"))]
    (str header "\n\n" body "\n")))
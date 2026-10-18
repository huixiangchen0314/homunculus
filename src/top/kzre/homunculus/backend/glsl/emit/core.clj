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
  (clang.lang/naming (:name node)))

(defmethod emit-node* :binary-op [node env]
  (clang.format/binary-op
    (:op node)
    (emit-node (:left node) env)
    (emit-node (:right node) env)))

(defmethod emit-node* :unary-op [node env]
  (clang.format/unary-op
    (:op node)
    (emit-node (:expr node) env)))

(defmethod emit-node* :call [node env]
  (clang.format/call
    (lang/naming-fn (:fn node))
    (map #(emit-node % env) (:args node))))

(defmethod emit-node* :member-access [node env]
  (clang.format/member-access
    (emit-node (:target node) env)
    (:member node)))

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
    :name (:name node)
    :type (:type node)
    :init (when-let [i (:init node)]
            (emit-node i env))))

(defmethod emit-node* :uniform [node _]
  (let [type-str (:type node)
        name     (:name node)]
    (T "uniform ${type-str} ${name};")))

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

(defmethod emit-node* :block [node env]
  (clang.format/block (indent/indent (emit-block-body node env))))

;; ═══════════════════════════════════════════
;; 顶层声明
;; ═══════════════════════════════════════════

(defmethod emit-node* :param [node _]
  (let [type-str (:type node)
        name     (:name node)]
    (T "${type-str} ${name}")))

(defmethod emit-node* :struct-member [node _]
  (clang.format/struct-member
    :name (:name node)
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
  (let [name      (:name node)
        layout-parts (cond-> []
                             (:packing node) (conj (clang.lang/naming (:packing node)))
                             (:binding node) (conj (str "binding = " (:binding node))))
        layout-str (if (seq layout-parts)
                     (let [joined (str/join ", " layout-parts)]
                       (str "layout(" joined ") "))
                     "")
        members   (str/join "\n"
                            (map #(emit-node % env) (:members node)))
        members   (indent/indent members)]
    (T "${layout-str}uniform ${name} {\n${members}\n};")))

(defmethod emit-node* :interface-block [node _env]
  (throw (ex-info "interface-block emission not yet implemented"
                  {:name (:name node)})))

(defmethod emit-node* :function [node env]
  (clang.format/function
    :name (:name node)
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
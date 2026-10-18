(ns top.kzre.homunculus.backend.glsl.lower.core
  "ShaderAST（HLSL 外观）→ GLSL AST 的降级。

   大部分节点字段名与 GLSL AST 对齐——直接用 map-> 构造，
   只对子节点做递归降级、对 :type 做类型字符串转换。少数节点
   有结构性改写（采样调用、cbuffer、entry-point、static-var）。

   ── 入口点策略 ────────────────────────────────────────
   :wrap    —— 原 entry 改名 __entry_<name>，生成 wrapper main
   :inplace —— 原地改造 entry 为 main（暂未实现）"
  (:require
    [top.kzre.homunculus.backend.glsl.ast :as glsl]
    [top.kzre.homunculus.backend.glsl.lang :as lang]
    [top.kzre.homunculus.backend.glsl.lower.env :as env]
    [top.kzre.homunculus.backend.hlsl.lang :as hlsl.lang]
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.backend.glsl.sematic.layout]
    [top.kzre.homunculus.core.ir2.pass.type :as ty])
  (:import
    (top.kzre.homunculus.backend.glsl.sematic.layout
      Layout SamplerUniform IODecl EntryLayout)))

;; ═══════════════════════════════════════════
;; AST 辅助
;; ═══════════════════════════════════════════

(defn- find-struct-def
  [asts struct-sym]
  (some (fn [n]
          (when (and (= :struct (ast/kind n))
                     (= struct-sym (:name n)))
            n))
        asts))

(declare lower-node)

(defn- lower-children
  [children layout]
  (mapv #(lower-node % layout) children))

;; ═══════════════════════════════════════════
;; 采样调用改写
;; ═══════════════════════════════════════════

(defn- find-sampler
  [^Layout layout tex-name smp-name]
  (or (some (fn [^SamplerUniform s]
              (when (and (= (:texture s) tex-name)
                         (= (:sampler s) smp-name))
                s))
            (:samplers layout))
      (throw (ex-info "no sampler uniform for (texture, sampler)"
                      {:texture tex-name :sampler smp-name}))))

(def ^:private sample-fn->glsl
  {'sample             "texture"
   'sampleLod          "textureLod"
   'sampleBias         "texture"
   'sampleCmp          "texture"
   'sampleCmpLevelZero "textureLod"
   'sampleGrad         "textureGrad"
   'gather             "textureGather"
   'gatherRed          "textureGather"
   'gatherGreen        "textureGather"
   'gatherBlue         "textureGather"
   'gatherAlpha        "textureGather"})

(def ^:private gather-component
  {'gather      nil
   'gatherRed   0
   'gatherGreen 1
   'gatherBlue  2
   'gatherAlpha 3})

;; ═══════════════════════════════════════════
;; 节点降级
;; ═══════════════════════════════════════════

(defmulti lower-node*
          (fn [node _layout] (ast/kind node)))

(defn- lower-node
  [node layout]
  (lower-node* node layout))

;; ── 表达式 ────────────────────────────────────

(defmethod lower-node* :literal [node _]
  (glsl/map->Literal node))

(defmethod lower-node* :variable [node _]
  (glsl/map->Variable node))

(defmethod lower-node* :binary-op [node layout]
  (glsl/map->BinaryOp
    (-> node
        (update :left lower-node layout)
        (update :right lower-node layout))))

(defmethod lower-node* :unary-op [node layout]
  (glsl/map->UnaryOp
    (update node :expr lower-node layout)))

(defmethod lower-node* :member-access [node layout]
  (glsl/map->MemberAccess
    (update node :target lower-node layout)))

(defmethod lower-node* :array-index [node layout]
  (glsl/map->ArrayIndex
    (-> node
        (update :target lower-node layout)
        (update :index lower-node layout))))

(defmethod lower-node* :constructor [node layout]
  (glsl/map->Constructor
    (-> node
        (update :type lang/type->str)
        (update :args lower-children layout))))

(defmethod lower-node* :cast [node layout]
  (glsl/map->Cast
    (-> node
        (update :type lang/type->str)
        (update :expr lower-node layout))))

(defmethod lower-node* :call [node layout]
  (let [fn-name (:fn node)
        args    (:args node)]
    (if (hlsl.lang/sample-fn? fn-name)
      (let [[tex-node smp-node & rest-args] args
            tex-name (:name tex-node)
            smp-name (:name smp-node)
            sampler  (find-sampler layout tex-name smp-name)
            glsl-fn  (get sample-fn->glsl fn-name)
            component (get gather-component fn-name)
            lowered-args (lower-children rest-args layout)
            lowered-args (if (some? component)
                           (conj lowered-args
                                 (glsl/->Literal component nil nil))
                           lowered-args)
            new-args     (into [(glsl/->Variable (:var-name sampler) nil nil)]
                               lowered-args)]
        (glsl/map->Call
          (-> node
              (assoc :fn glsl-fn)
              (assoc :args new-args))))
      (glsl/map->Call
        (update node :args lower-children layout)))))

;; ── 语句 ──────────────────────────────────────

(defmethod lower-node* :var-decl [node layout]
  (glsl/map->VarDecl
    (-> node
        (update :type lang/type->str)
        (update :init #(some-> % (lower-node layout))))))

(defmethod lower-node* :assign [node layout]
  (glsl/map->Assign
    (-> node
        (update :lhs lower-node layout)
        (update :rhs lower-node layout))))

(defmethod lower-node* :if [node layout]
  (glsl/map->If
    (-> node
        (update :test lower-node layout)
        (update :then lower-node layout)
        (update :else #(some-> % (lower-node layout))))))

(defmethod lower-node* :while [node layout]
  (glsl/map->While
    (-> node
        (update :test lower-node layout)
        (update :body lower-node layout))))

(defmethod lower-node* :block [node layout]
  (glsl/map->Block
    (-> node
        (update :stmts lower-children layout)
        (update :ret #(some-> % (lower-node layout))))))

;; ── 顶层声明 ──────────────────────────────────

(defmethod lower-node* :function [node layout]
  (glsl/map->Function
    (-> node
        (update :return-type lang/type->str)
        (update :params #(mapv (fn [p]
                                 (glsl/map->Param
                                   (update p :type lang/type->str)))
                               %))
        (update :body lower-node layout))))

(defmethod lower-node* :struct [node _]
  (glsl/map->Struct
    (update node :members
            #(mapv (fn [m]
                     (glsl/map->StructMember
                       (update m :type lang/type->str)))
                   %))))

(defmethod lower-node* :cbuffer-decl [node _]
  (let [binding (when-let [reg (:register node)]
                  (parse-long (subs (name reg) 1)))
        members (mapv (fn [m]
                        (glsl/map->StructMember
                          (update m :type lang/type->str)))
                      (:members node))]
    (glsl/->UniformBlock (:name node) binding :std140 members
                         (:attrs node) (:meta node))))

(defmethod lower-node* :uniform [node _]
  (glsl/map->Uniform
    (update node :type lang/type->str)))

(defmethod lower-node* :static-var [node layout]
  (glsl/map->GlobalVar
    (-> node
        (update :type lang/type->str)
        (update :init #(some-> % (lower-node layout)))
        (assoc :const? false))))

;; ── texture / sampler 已被 sematic 规划处理 ──

(defmethod lower-node* :texture-decl [_ _] nil)
(defmethod lower-node* :sampler-decl [_ _] nil)

;; ═══════════════════════════════════════════
;; 声明生成
;; ═══════════════════════════════════════════

(defn- lower-samplers
  [^Layout layout]
  (mapv (fn [^SamplerUniform s]
          (glsl/->Uniform (:var-name s) (:sampler-type-str s) nil nil))
        (:samplers layout)))

(defn- lower-input-decls
  [^EntryLayout entry]
  (into []
        (keep (fn [^IODecl d]
                (when-not (:builtin? d)
                  (glsl/->InputDecl (:var-name d)
                                    (:location d)
                                    (lang/type->str (:type d))
                                    (:flat? d)
                                    nil
                                    nil))))
        (:inputs entry)))

(defn- lower-output-decls
  [^EntryLayout entry]
  (into []
        (keep (fn [^IODecl d]
                (when-not (:builtin? d)
                  (glsl/->OutputDecl (:var-name d)
                                     (:location d)
                                     (lang/type->str (:type d))
                                     (:flat? d)
                                     nil
                                     nil))))
        (:outputs entry)))

;; ═══════════════════════════════════════════
;; 入口点降级
;; ═══════════════════════════════════════════

(defn- rename-entry [name]
  (str "__entry_" name))

(defn- build-wrapper-call
  [original-name ^EntryLayout entry]
  (glsl/->Call original-name
               (mapv (fn [^IODecl in]
                       (glsl/->Variable (:var-name in) nil nil))
                     (:inputs entry))
               nil
               nil))

(defn- build-wrapper-body
  [call-expr node ^EntryLayout layout-entry struct-def]
  (let [outputs (:outputs layout-entry)]
    (if (= 1 (count outputs))
      (let [^IODecl out (first outputs)]
        [(glsl/->Assign (glsl/->Variable (:var-name out) nil nil)
                        call-expr
                        nil
                        nil)])
      (let [tmp-name    "__entry_result"
            tmp-type    (lang/type->str (:return-type node))
            tmp-decl    (glsl/->VarDecl tmp-name tmp-type call-expr nil nil)
            field-names (mapv :name (:members struct-def))]
        (into [tmp-decl]
              (mapv (fn [^IODecl out field-name]
                      (glsl/->Assign
                        (glsl/->Variable (:var-name out) nil nil)
                        (glsl/->MemberAccess
                          (glsl/->Variable tmp-name nil nil)
                          field-name
                          nil
                          nil)
                        nil
                        nil))
                    outputs field-names))))))

(defn- build-wrapper
  [node ^EntryLayout layout-entry asts]
  (let [return-type (:return-type node)
        struct-sym  (ty/type-sym return-type)
        struct-def  (when struct-sym
                      (find-struct-def asts struct-sym))
        call-expr   (build-wrapper-call (rename-entry (:name node)) layout-entry)
        body-stmts  (build-wrapper-body call-expr node layout-entry struct-def)]
    (glsl/->Function "main" "void" []
                     (glsl/->Block body-stmts nil nil nil)
                     nil
                     nil)))

(defn- lower-entry-wrap
  [node layout asts]
  (let [entry-layout  (:entry layout)
        original-fn   (glsl/map->Function
                        (-> node
                            (update :name rename-entry)
                            (update :return-type lang/type->str)
                            (update :params #(mapv (fn [p]
                                                     (glsl/map->Param
                                                       (update p :type lang/type->str)))
                                                   %))
                            (update :body lower-node layout)))
        wrapper       (build-wrapper node entry-layout asts)]
    [original-fn wrapper]))

(defn- lower-entry-inplace
  [_node _layout _asts]
  (throw (ex-info "inplace entry strategy not yet implemented" {})))

(defn- lower-entry
  [node env layout asts]
  (let [entry-layout (:entry layout)
        in-decls     (lower-input-decls entry-layout)
        out-decls    (lower-output-decls entry-layout)
        entry-fns    (case (env/low-entry-strategy env)
                       :wrap    (lower-entry-wrap node layout asts)
                       :inplace (lower-entry-inplace node layout asts)
                       (throw (ex-info "unknown low-entry-strategy"
                                       {:strategy (env/low-entry-strategy env)})))]
    (into (into in-decls out-decls) entry-fns)))

;; ═══════════════════════════════════════════
;; 主入口
;; ═══════════════════════════════════════════

(defn lower
  [asts env ^Layout layout]
  (let [entries     (filter #(= :entry-point (ast/kind %)) asts)
        non-entries (remove #(= :entry-point (ast/kind %)) asts)]
    (when (empty? entries)
      (throw (ex-info "no entry-point in asts" {})))
    (when (> (count entries) 1)
      (throw (ex-info "multiple entry-points in a single group"
                      {:count (count entries)})))
    (let [entry-node    (first entries)
          sampler-decls (lower-samplers layout)
          decls         (into []
                              (keep #(lower-node % layout))
                              non-entries)
          entry-decls   (lower-entry entry-node env layout asts)]
      (into []
            (concat sampler-decls decls entry-decls)))))
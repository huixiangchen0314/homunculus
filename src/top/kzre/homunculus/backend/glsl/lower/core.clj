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
   [top.kzre.homunculus.backend.glsl.sematic.layout]
   [top.kzre.homunculus.backend.glsl.sematic.layout :as layout]
   [top.kzre.homunculus.backend.hlsl.lang :as hlsl.lang]
   [top.kzre.homunculus.backend.shader.ast :as ast]
   [top.kzre.homunculus.core.ir2.pass.type :as ty])
  (:import
    (top.kzre.homunculus.backend.glsl.sematic.layout
    EntryLayout
    IODecl
    Layout
    SamplerUniform)))

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
    (cond
      (hlsl.lang/sample-fn? fn-name)
      (let [[tex-node smp-node & rest-args] args
            tex-name (:name tex-node)
            smp-name (:name smp-node)
            sampler  (layout/find-sampler layout tex-name smp-name)
            glsl-fn  (lang/sample-fn-str fn-name)
            component (lang/sample-grater-component fn-name)
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


      (= (name fn-name) "load")
      (let [[tex-node & coords] args
            tex-name (:name tex-node)
            sampler  (layout/find-load-sampler layout tex-name)
            lowered-coords (lower-children coords layout)
            dimension (:dimension sampler)
            ;; Buffer 没有 lod 参数——其他维度补 0
            final-coords (if (= :buffer dimension)
                           lowered-coords
                           (if (= 1 (count lowered-coords))
                             (conj lowered-coords (glsl/->Literal 0 nil nil))
                             lowered-coords))]
        (glsl/map->Call
          (-> node
              (assoc :fn (symbol "texelFetch"))
              (assoc :args (into [(glsl/->Variable (:var-name sampler) nil nil)]
                                 final-coords)))))
      :else
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

(defn- wrapper-call
  "构造 wrapper 对原 entry 函数的调用。

   inputs 按位置映射为实参——builtin 输入（如 gl_FragCoord）
   也传。原 entry 函数的形参保留了这些位置——函数体里可能引用。"
  [original-name ^EntryLayout entry]
  (let [args (mapv (fn [^IODecl in]
                     ;; TODO InterfaceBlock 支持
                     (glsl/->Variable (:var-name in) nil nil))
                   (:inputs entry))]
    (glsl/->Call (symbol original-name) args nil nil)))

(defn- wrapper-single-body
  "单 out——直接赋值。builtin out（gl_Position / gl_FragDepth）
   照常赋值。"
  [call-expr ^IODecl out]
  [(glsl/->Assign (glsl/->Variable (:var-name out) nil nil)
                  call-expr nil nil)])

(defn- wrapper-struct-body
  "多 out——struct 返回值赋给临时变量，再逐字段赋给对应 out。

   field-names 与 outputs 按位置对应——都由 plan-struct-fields
   按 struct 成员顺序产出。"
  [call-expr return-type ^EntryLayout entry struct-def]
  (let [tmp-name    "__entry_result"
        tmp-type    (lang/type->str return-type)
        tmp-decl    (glsl/->VarDecl tmp-name tmp-type call-expr nil nil)
        field-names (mapv :name (:members struct-def))]
    (into [tmp-decl]
          (map (fn [^IODecl out field-name]
                 (glsl/->Assign
                   (glsl/->Variable (:var-name out) nil nil)
                   (glsl/->MemberAccess (glsl/->Variable tmp-name nil nil)
                                        field-name nil nil)
                   nil nil))
               (:outputs entry) field-names))))

(defn- wrapper-body
  [call-expr node ^EntryLayout entry struct-def]
  (if (= 1 (count (:outputs entry)))
    (wrapper-single-body call-expr (first (:outputs entry)))
    (if struct-def
      (wrapper-struct-body call-expr (:return-type node) entry struct-def)
      (throw (ex-info "struct return type definition required for multiple outputs"
                      {:return-type (:return-type node)})))))

(defn- build-wrapper
  [node ^EntryLayout entry asts]
  (let [return-type (:return-type node)
        struct-sym  (ty/type-sym return-type)
        struct-def  (when struct-sym
                      (find-struct-def asts struct-sym))
        call-expr   (wrapper-call (rename-entry (:name node)) entry)
        body-stmts  (wrapper-body call-expr node entry struct-def)]
    (glsl/->Function "main" "void" []
                     (glsl/->Block body-stmts nil nil nil)
                     nil nil)))

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
(ns top.kzre.homunculus.backend.glsl.lower.core
  "ShaderAST（HLSL 外观）→ GLSL AST 的降级。

   ── 分层 ──────────────────────────────────────────────
   ▸ 逐节点翻译——表达式、语句、function、struct、cbuffer、
     uniform、global-var：节点结构一对一映射。
   ▸ 结构重分配——texture/sampler 已被 sematic 规划合并为
     sampler uniform。降级器跳过原声明，按 layout.samplers 生成
     uniform 声明；采样调用改写为 texture() 等 GLSL 内置函数。
   ▸ 语义降级——entry-point 按 layout.entry 展开为全局 in/out
     声明 + wrapper main（:wrap）或原地改造（:inplace）。

   ── 入口点策略 ────────────────────────────────────────
   :wrap    —— 原 entry 改名为 __entry_<name>，生成 wrapper main
   :inplace —— 原地改造 entry 为 main（暂未实现）"
  (:require
    [top.kzre.homunculus.backend.glsl.ast :as glsl]
    [top.kzre.homunculus.backend.glsl.lang :as lang]
    [top.kzre.homunculus.backend.glsl.lower.env :as env]
    [top.kzre.homunculus.backend.glsl.sematic.layout]
    [top.kzre.homunculus.backend.hlsl.lang :as hlsl.lang]
    [top.kzre.homunculus.backend.shader.ast :as ast]
    [top.kzre.homunculus.core.ir2.pass.type :as ty])
  (:import
    (top.kzre.homunculus.backend.glsl.sematic.layout
      Layout SamplerUniform IODecl EntryLayout)))

;; ═══════════════════════════════════════════
;; AST 辅助
;; ═══════════════════════════════════════════

(defn- find-struct-def
  "从 asts 里查 struct 定义。找不到返回 nil。"
  [asts struct-sym]
  (some (fn [n]
          (when (and (= :struct (ast/kind n))
                     (= struct-sym (:name n)))
            n))
        asts))

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
;; 节点降级——多方法分派
;; ═══════════════════════════════════════════

(declare lower-node)

(defmulti lower-node*
          (fn [node _layout] (ast/kind node)))

(defn- lower-node
  [node layout]
  (lower-node* node layout))

(defn- lower-children
  [children layout]
  (mapv #(lower-node % layout) children))

;; ── 表达式 ────────────────────────────────────

(defmethod lower-node* :literal [node _]
  (glsl/->Literal (:val node) (:attrs node)))

(defmethod lower-node* :variable [node _]
  (glsl/->Variable (:name node) (:attrs node)))

(defmethod lower-node* :binary-op [node layout]
  (glsl/->BinaryOp (:op node)
                   (lower-node (:left node) layout)
                   (lower-node (:right node) layout)
                   (:attrs node)))

(defmethod lower-node* :unary-op [node layout]
  (glsl/->UnaryOp (:op node)
                  (lower-node (:expr node) layout)
                  (:attrs node)))

(defmethod lower-node* :member-access [node layout]
  (glsl/->MemberAccess (lower-node (:target node) layout)
                       (:member node)
                       (:attrs node)))

(defmethod lower-node* :array-index [node layout]
  (glsl/->ArrayIndex (lower-node (:target node) layout)
                     (lower-node (:index node) layout)
                     (:attrs node)))

(defmethod lower-node* :constructor [node layout]
  (glsl/->Constructor (lang/type->str (:type node))
                      (lower-children (:args node) layout)
                      (:attrs node)))

(defmethod lower-node* :cast [node layout]
  ;; GLSL 无 C-style cast——降级为构造函数语法
  (glsl/->Cast (lang/type->str (:type node))
               (lower-node (:expr node) layout)
               (:attrs node)))

(defmethod lower-node* :call [node layout]
  (let [fn-name (:fn node)
        args    (:args node)]
    (if (hlsl.lang/sample-fn? fn-name)
      ;; 采样调用——tex.Sample(samp, ...) → texture(tex_samp, ...)
      (let [[tex-node smp-node & rest-args] args
            tex-name (:name tex-node)
            smp-name (:name smp-node)
            sampler  (find-sampler layout tex-name smp-name)
            glsl-fn  (get sample-fn->glsl fn-name)
            component (get gather-component fn-name)
            lowered-args (lower-children rest-args layout)
            lowered-args (if (some? component)
                           (conj lowered-args (glsl/->Literal component nil))
                           lowered-args)]
        (glsl/->Call glsl-fn
                     (into [(glsl/->Variable (:var-name sampler) nil)]
                           lowered-args)
                     (:attrs node)))
      ;; 非采样调用——逐节点翻译
      (glsl/->Call fn-name
                   (lower-children args layout)
                   (:attrs node)))))

;; ── 语句 ──────────────────────────────────────

(defmethod lower-node* :var-decl [node layout]
  (glsl/->VarDecl (:name node)
                  (lang/type->str (:type node))
                  (some-> (:init node) (lower-node layout))
                  (:attrs node)))

(defmethod lower-node* :assign [node layout]
  (glsl/->Assign (lower-node (:lhs node) layout)
                 (lower-node (:rhs node) layout)
                 (:attrs node)))

(defmethod lower-node* :if [node layout]
  (glsl/->If (lower-node (:test node) layout)
             (lower-node (:then node) layout)
             (some-> (:else node) (lower-node layout))
             (:attrs node)))

(defmethod lower-node* :while [node layout]
  (glsl/->While (lower-node (:test node) layout)
                (lower-node (:body node) layout)
                (:attrs node)))

(defmethod lower-node* :block [node layout]
  (glsl/->Block (lower-children (:stmts node) layout)
                (some-> (:ret node) (lower-node layout))
                (:attrs node)))

;; ── 顶层声明 ──────────────────────────────────

(defmethod lower-node* :function [node layout]
  (glsl/->Function (:name node)
                   (lang/type->str (:return-type node))
                   (mapv (fn [p]
                           (glsl/->Param (:name p)
                                         (lang/type->str (:type p))
                                         (:attrs p)))
                         (:params node))
                   (lower-node (:body node) layout)
                   (:attrs node)))

(defmethod lower-node* :struct [node _]
  (glsl/->Struct (:name node)
                 (mapv (fn [m]
                         (glsl/->StructMember (:name m)
                                              (lang/type->str (:type m))
                                              (:attrs m)))
                       (:members node))
                 (:attrs node)))

(defmethod lower-node* :cbuffer-decl [node _]
  (let [binding (when-let [reg (:register node)]
                  (parse-long (subs (name reg) 1)))]
    (glsl/->UniformBlock (:name node)
                         binding
                         :std140
                         (mapv (fn [m]
                                 (glsl/->StructMember (:name m)
                                                      (lang/type->str (:type m))
                                                      (:attrs m)))
                               (:members node))
                         (:attrs node))))

(defmethod lower-node* :uniform [node _]
  (glsl/->Uniform (:name node)
                  (lang/type->str (:type node))
                  (:attrs node)))

(defmethod lower-node* :static-var [node layout]
  (glsl/->GlobalVar (:name node)
                    (lang/type->str (:type node))
                    (some-> (:init node) (lower-node layout))
                    false
                    (:attrs node)))

;; ── texture / sampler 已被 sematic 规划处理 ──

(defmethod lower-node* :texture-decl [_ _] nil)
(defmethod lower-node* :sampler-decl [_ _] nil)

;; ═══════════════════════════════════════════
;; 声明生成
;; ═══════════════════════════════════════════

(defn- lower-samplers
  [^Layout layout]
  (mapv (fn [^SamplerUniform s]
          (glsl/->Uniform (:var-name s) (:sampler-type-str s) nil))
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
                       (glsl/->Variable (:var-name in) nil))
                     (:inputs entry))
               nil))

(defn- build-wrapper-body
  "构造 wrapper main 的函数体。

   单 out：out = __entry_<name>(ins...);
   多 out：T tmp = __entry_<name>(ins...);
           out1 = tmp.field1; out2 = tmp.field2; ...

   struct 返回的多 out 场景——通过 struct-def 获取字段名。
   IODecl.var-name 是 GLSL 全局名（可能是 gl_Position 或自定义名），
   而 struct 字段访问用原始字段名。"
  [call-expr node ^EntryLayout layout-entry struct-def]
  (let [outputs (:outputs layout-entry)]
    (if (= 1 (count outputs))
      (let [^IODecl out (first outputs)]
        [(glsl/->Assign (glsl/->Variable (:var-name out) nil)
                        call-expr
                        nil)])
      (let [tmp-name    "__entry_result"
            tmp-type    (lang/type->str (:return-type node))
            tmp-decl    (glsl/->VarDecl tmp-name tmp-type call-expr nil)
            field-names (mapv :name (:members struct-def))]
        (into [tmp-decl]
              (mapv (fn [^IODecl out field-name]
                      (glsl/->Assign
                        (glsl/->Variable (:var-name out) nil)
                        (glsl/->MemberAccess
                          (glsl/->Variable tmp-name nil)
                          field-name
                          nil)
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
                     (glsl/->Block body-stmts nil nil)
                     nil)))

(defn- lower-entry-wrap
  "wrap 策略：原 entry 改名 __entry_<name> 保留，生成 wrapper main。"
  [node layout asts]
  (let [entry-layout  (:entry layout)
        original-fn   (glsl/->Function
                        (rename-entry (:name node))
                        (lang/type->str (:return-type node))
                        (mapv (fn [p]
                                (glsl/->Param (:name p)
                                              (lang/type->str (:type p))
                                              (:attrs p)))
                              (:params node))
                        (lower-node (:body node) layout)
                        (:attrs node))
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
  "把 HLSL 外观的 ShaderAST 组降级成 GLSL AST 列表。

   asts   —— 当前入口点组的 ShaderAST（恰好一个 entry-point）
   env    —— 降级环境
   layout —— sematic 规划结果

   返回 GLSL AST 列表：
     1. sampler uniform 声明（从 layout 生成）
     2. 原 asts 里非 entry 节点的降级（struct / function / uniform / ...）
     3. 入口点的 in/out 声明 + 原 entry 函数 + wrapper main"
  [asts ^env/IEnv env ^Layout layout]
  (let [entries     (filter #(= :entry-point (ast/kind %)) asts)
        non-entries (remove #(= :entry-point (ast/kind %)) asts)]
    (when (empty? entries)
      (throw (ex-info "no entry-point in asts" {})))
    (when (> (count entries) 1)
      (throw (ex-info "multiple entry-points in a single group"
                      {:count (count entries)})))
    (let [entry-node (first entries)
          sampler-decls (lower-samplers layout)
          decls         (into []
                              (keep #(lower-node % layout))
                              non-entries)
          entry-decls   (lower-entry entry-node env layout asts)]
      (into []
            (concat sampler-decls decls entry-decls)))))
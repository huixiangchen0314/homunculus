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
    [top.kzre.homunculus.backend.glsl.semantic.layout :as layout]
    [top.kzre.homunculus.backend.hlsl.lang :as hlsl.lang]
    [top.kzre.homunculus.frontend.shader.ast :as ast]
    [top.kzre.homunculus.frontend.shader.semantic :as semantic]
    [top.kzre.homunculus.core.ir2.pass.type :as ty])
  (:import
    (top.kzre.homunculus.backend.glsl.semantic.layout
      EntryLayout
      IODecl
      InterfaceBlock
      InterfaceBlockMember
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

      (hlsl.lang/texel-fetch-fn? fn-name)
      (let [[tex-node & coords] args
            tex-name (:name tex-node)
            sampler  (layout/find-load-sampler layout tex-name)
            lowered-coords (lower-children coords layout)
            dimension (:dimension sampler)
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

(defmethod lower-node* :for [node layout]
  (glsl/map->For
    (-> node
        (update :init #(some-> % (lower-node layout)))
        (update :test #(lower-node % layout))
        (update :update #(some-> % (lower-node layout)))
        (update :body #(lower-node % layout)))))

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

(defn- lower-decl
  "把一个规划声明（IODecl 或 InterfaceBlock）降级为 GLSL AST 节点。
   builtin IODecl 返回 nil——不声明，由 wrapper 直接读写内置变量。"
  [d direction]
  (cond
    (instance? IODecl d)
    (when-not (:builtin? d)
      (let [ctor (case direction
                   :in  glsl/->InputDecl
                   :out glsl/->OutputDecl)]
        (ctor (:var-name d)
              (:location d)
              (lang/type->str (:type d))
              (:flat? d)
              nil
              nil)))

    (instance? InterfaceBlock d)
    (glsl/->InterfaceBlock
      (:block-name d)
      direction
      (:instance-name d)
      (:location d)
      (mapv (fn [m]
              (glsl/->InterfaceBlockMember
                (:var-name m)
                (:location m)
                (lang/type->str (:type m))
                (:flat? m)
                nil
                nil))
            (:members d))
      nil
      nil)

    :else nil))

(defn- lower-decls
  [decls direction]
  (into [] (keep #(lower-decl % direction)) decls))

;; ═══════════════════════════════════════════
;; 入口点降级
;; ═══════════════════════════════════════════

(defn- rename-entry [name]
  (str "cljh_entry_" name))

(defn- require-struct-def
  [struct-def return-type]
  (or struct-def
      (throw (ex-info "struct return type definition required for wrapper"
                      {:return-type return-type}))))

(defn- build-by-sem
  "构建 semantic → 取值表达式构造器的映射。

   来源：
   ▸ IODecl —— 取 (:var-name) 作为 Variable（builtin 时是内置变量名）
   ▸ InterfaceBlock 成员 —— 取 (:instance-name) 的成员访问

   同一 semantic 在两个来源里同时出现时后写入的覆盖——
   plan 阶段已保证 builtin semantic 只出现在 IODecl 侧，
   block 只收非 builtin 字段，不会冲突。"
  [inputs]
  (let [block (some #(when (instance? InterfaceBlock %) %) inputs)]
    (into {}
          (concat
            (keep (fn [d]
                    (when (instance? IODecl d)
                      [(:semantic d)
                       (fn [] (glsl/->Variable (:var-name d) nil nil))]))
                  inputs)
            (when block
              (keep (fn [m]
                      (when-let [s (:semantic m)]
                        [s (fn []
                             (glsl/->MemberAccess
                               (glsl/->Variable (:instance-name block) nil nil)
                               (:var-name m) nil nil))]))
                    (:members block)))))))

(defn- wrapper-call
  "构造对原 entry 函数的调用。返回 [setup-stmts call-expr]。

   struct 参数（params 含非 primitive 类型）——构造完整 struct 传一个实参。
     字段按 semantic 从 IODecl 或 block 成员取值。
   离散参数（params 全 primitive）——每个 param 按 semantic 从 IODecl
     或 block 成员取值。

   判据是 params 的构成而不是 inputs 的构成——VS 用 block 输出 + FS
   离散参数时，inputs 有 block 但 params 全 primitive；:split 策略下
   FS struct 参数时，inputs 全离散 IODecl 但 params 含 struct。"
  [node ^EntryLayout entry asts]
  (let [original-name (rename-entry (:name node))
        params        (:params node)
        inputs        (:inputs entry)
        by-sem        (build-by-sem inputs)
        struct-param? (some #(not (lang/primitive-type? (:type %))) params)]
    (if struct-param?
      ;; ── struct 参数：构造完整 struct ──
      (let [p0         (first params)
            struct-def (find-struct-def asts (ty/type-sym (:type p0)))]
        (when-not struct-def
          (throw (ex-info "struct definition not found for param"
                          {:param (:name p0) :type (:type p0)})))
        (let [ctor-args (mapv (fn [f]
                                (let [sem  (semantic/shader-semantic f)
                                      ctor (get by-sem sem)]
                                  (when-not ctor
                                    (throw (ex-info "no input matched for struct field semantic"
                                                    {:field (:name f) :semantic sem})))
                                  (ctor)))
                              (:members struct-def))
              tmp       "cljh_entry_input"
              type-str  (lang/type->str (:type p0))
              setup     [(glsl/->VarDecl
                           tmp type-str
                           (glsl/->Constructor type-str ctor-args nil nil)
                           nil nil)]]
          [setup (glsl/->Call (symbol original-name)
                              [(glsl/->Variable tmp nil nil)] nil nil)]))

      ;; ── 离散参数：按 semantic 取 ──
      (let [args (mapv (fn [p]
                         (let [sem  (semantic/shader-semantic p)
                               ctor (get by-sem sem)]
                           (when-not ctor
                             (throw (ex-info "no input matched for param semantic"
                                             {:param (:name p) :semantic sem})))
                           (ctor)))
                       params)]
        [[] (glsl/->Call (symbol original-name) args nil nil)]))))

(defn- wrapper-single-body
  [call-expr ^IODecl out]
  [(glsl/->Assign (glsl/->Variable (:var-name out) nil nil)
                  call-expr nil nil)])

(defn- wrapper-struct-body
  [call-expr return-type ^EntryLayout entry struct-def]
  (let [struct-def  (require-struct-def struct-def return-type)
        tmp-name    "cljh_entry_result"
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

(defn- wrapper-interface-body
  [call-expr return-type ^EntryLayout entry struct-def]
  (let [struct-def  (require-struct-def struct-def return-type)
        stage       (:stage entry)
        tmp-name    "cljh_entry_result"
        tmp-type    (lang/type->str return-type)
        tmp-decl    (glsl/->VarDecl tmp-name tmp-type call-expr nil nil)
        block       (some #(when (instance? InterfaceBlock %) %) (:outputs entry))
        assigns     (mapv (fn [f]
                            (let [sem   (semantic/shader-semantic f)
                                  value (glsl/->MemberAccess
                                          (glsl/->Variable tmp-name nil nil)
                                          (:name f) nil nil)]
                              (if (lang/builtin-semantic? stage sem)
                                (glsl/->Assign
                                  (glsl/->Variable
                                    (lang/builtin-var-name stage sem) nil nil)
                                  value nil nil)
                                (glsl/->Assign
                                  (glsl/->MemberAccess
                                    (glsl/->Variable (:instance-name block) nil nil)
                                    (:name f) nil nil)
                                  value nil nil))))
                          (:members struct-def))]
    (into [tmp-decl] assigns)))

(defn- wrapper-body
  [call-expr node ^EntryLayout entry struct-def]
  (cond
    (some #(instance? InterfaceBlock %) (:outputs entry))
    (wrapper-interface-body call-expr (:return-type node) entry struct-def)

    (= 1 (count (:outputs entry)))
    (wrapper-single-body call-expr (first (:outputs entry)))

    struct-def
    (wrapper-struct-body call-expr (:return-type node) entry struct-def)

    :else
    (throw (ex-info "struct return type definition required for multiple outputs"
                    {:return-type (:return-type node)}))))

(defn- build-wrapper
  [node ^EntryLayout entry asts]
  (let [return-type (:return-type node)
        struct-sym  (ty/type-sym return-type)
        struct-def  (when struct-sym
                      (find-struct-def asts struct-sym))
        [setup call-expr] (wrapper-call node entry asts)
        body-stmts  (into setup (wrapper-body call-expr node entry struct-def))]
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
        in-decls     (lower-decls (:inputs entry-layout) :in)
        out-decls    (lower-decls (:outputs entry-layout) :out)
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
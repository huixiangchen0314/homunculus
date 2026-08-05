(ns top.kzre.homunculus.backend.shader.lower.core
  "IR2 → ShaderAST 降级器。产出纯粹的 ShaderAST 节点，类型保留为 IR 类型对象。"
  (:require
   [top.kzre.homunculus.backend.shader.ast :as ast]
   [top.kzre.homunculus.core.ir2.node :as n]
   [top.kzre.homunculus.backend.shader.metadata :as md]
   [top.kzre.homunculus.core.types.type :as ty]))

(declare lower-node lower-body)

;; ── 辅助 ──
(defn- ir-type [node] (ty/get-type node))
(defn- var-name [node] (name (:name node)))

;; ── 顶层降级 ──
(defmulti lower-top (fn [node _ctx] (n/kind node)))

(defmethod lower-top :ns [node _ctx]
  (let [deps (:references node)]
    (mapv (fn [dep-sym] (ast/->Import dep-sym)) deps)))

;; ── 顶层：资源声明 ──
(defmethod lower-top :define [node ctx]
  (let [meta (n/node-meta node)]
    (if-let [res-kind (:shader/resource-kind meta)]
      ;; ── 资源声明 ──
      (let [res-name (name (n/define-name node))
            slot     (case res-kind
                       :texture2D (:shader/texture-register meta)
                       :sampler   (:shader/sampler-register meta)
                       :cbuffer   (:shader/cbuffer-register meta)
                       nil)
            members  (when (= res-kind :cbuffer)
                       (mapv (fn [[sym type-sym]]
                               (ast/->StructMember (name sym) (ty/make-tcon type-sym)))
                             (:shader/cbuffer-members meta)))]
        (ast/->ResourceDecl res-name res-kind slot members))

      ;; ── 非资源：普通函数或变量 ──
      (let [val (n/define-val node)]
        (if (and val (= :lambda (n/kind val)))
          ;; 函数 / 入口点
          (let [lam   val
                stage (md/shader-stage node)      ; nil 或 :vertex/:fragment
                ret-ty (ty/fun-return-type (ir-type lam))
                params (n/lambda-params lam)
                param-nodes (mapv (fn [p] (ast/->Param (var-name p) (ir-type p))) params)
                [body-block ret-expr] (lower-body (n/lambda-body lam) ctx)]
            (if stage
              (ast/->EntryPoint (name (n/define-name node)) stage ret-ty param-nodes ret-expr body-block)
              (ast/->Function   (name (n/define-name node))        ret-ty param-nodes ret-expr body-block)))
          ;; 变量 / 常量
          (let [init (when val (lower-node val ctx))]
            (ast/->VarDecl (var-name node) (ir-type node) init)))))))


(defmethod lower-top :record [node _ctx]
  (let [struct-name (name (n/record-name node))
        fields (n/record-fields node)
        members (mapv (fn [f]
                        (ast/->StructMember (name (:name f)) (ir-type f)))
                      fields)]
    (ast/->Struct struct-name members)))

(defmethod lower-top :default [node _ctx]
  (throw (ex-info (str "Unsupported top-level IR2 node: " (n/kind node)) {:node node})))

;; ── 函数体处理：分离语句块与返回值 ──
(defn- lower-body [body-node ctx]
  (let [body (lower-node body-node ctx)]
    (if (= :block (ast/kind body))
      (let [stmts (:stmts body)
            cnt (count stmts)]
        (if (zero? cnt)
          [(ast/->Block []) nil]
          (let [last-s (last stmts)
                but-s (butlast stmts)]
            ;; 如果最后一条是语句（无值），则作为普通语句，无返回值
            (if (contains? #{:assign :var-decl :if :while :block} (ast/kind last-s))
              [body nil]
              [(ast/->Block (vec but-s)) last-s]))))
      ;; 单个表达式直接作为返回值
      [(ast/->Block []) body])))

;; ── 表达式降级 ──
(defmulti lower-expr (fn [node _ctx] (n/kind node)))

(defmethod lower-expr :literal [node _ctx]
  (ast/->Literal (n/lit-val node)))

(defmethod lower-expr :variable [node _ctx]
  (ast/->Variable (var-name node)))


;; ── 表达式：运算符识别 ──
(def ^:private infix-ops #{"+" "-" "*" "/" "%" "==" "!=" "<" ">" "<=" ">=" "&&" "||"})
(def ^:private unary-ops #{"!" "-" "++" "--"})

(defmethod lower-expr :call [node ctx]
  (let [fn-node (n/call-fn node)
        fn-name (if (and fn-node (= :variable (n/kind fn-node)))
                  (var-name fn-node)
                  nil)
        args (mapv #(lower-expr % ctx) (n/call-args node))]
    (cond
      ;; 一元运算符
      (and fn-name (contains? unary-ops fn-name) (= (count args) 1))
      (ast/->UnaryOp fn-name (first args))
      ;; 二元运算符
      (and fn-name (contains? infix-ops fn-name) (= (count args) 2))
      (ast/->BinaryOp fn-name (first args) (second args))
      ;; 普通调用
      :else
      (ast/->Call (or fn-name "<unknown>") args))))

(defmethod lower-expr :if [node ctx]
  ;; IR2 的 if 是表达式，但 ShaderAST 没有三元表达式，暂降级为 if 语句。
  ;; 实际使用中，这种 if 表达式通常在 IR 优化阶段被提升为语句，这里作为安全网。
  (let [test (lower-expr (n/if-test node) ctx)
        then-expr (lower-expr (n/if-then node) ctx)
        else-expr (when-let [e (n/if-else node)] (lower-expr e ctx))]
    (ast/->If test
              (ast/->Block [then-expr])
              (when else-expr (ast/->Block [else-expr])))))

(defmethod lower-expr :vector [node ctx]
  (let [items (mapv #(lower-expr % ctx) (n/vector-items node))
        vty (ir-type node)]
    (ast/->Constructor vty items)))

(defmethod lower-expr :member-access [node ctx]
  (let [target (lower-expr (n/access-target node) ctx)
        member (n/access-member node)
        args   (n/access-args node)]
    (cond
      (and (empty? args)
           (string? member)    ; 实际上 member 是关键字或符号，这里检查其名称
           (re-matches #"^[xyzwrgba]+$" (name member)))
      (ast/->Swizzle target (name member))
      (empty? args)
      (ast/->MemberAccess target member)
      :else
      (throw (ex-info "Method call not yet supported" {:node node})))))

(defmethod lower-expr :convert [node ctx]
  (let [dst-ty (n/convert-dst-ty node)
        expr (lower-expr (n/convert-expr node) ctx)]
    (ast/->Cast dst-ty expr)))

(defmethod lower-expr :new-array [node ctx]
  ;; 暂时映射为变量声明 + 循环初始化，由渲染器处理；这里简化为构造器
  (let [size (lower-expr (n/new-array-size node) ctx)]
    (ast/->Constructor (ir-type node) [size])))

(defmethod lower-expr :aget [node ctx]
  (ast/->ArrayIndex (lower-expr (n/aget-target node) ctx)
                    (lower-expr (n/aget-idx node) ctx)))

(defmethod lower-expr :aset [node ctx]
  ;; 作为赋值语句
  (ast/->Assign (ast/->ArrayIndex (lower-expr (n/aset-target node) ctx)
                                  (lower-expr (n/aset-idx node) ctx))
                (lower-expr (n/aset-val node) ctx)))

(defmethod lower-expr :alength [node _ctx]
  ;; 数组长度，若类型已知则降级为常量，否则保留为属性访问
  (let [target (n/alength-target node)
        target-ty (ir-type target)]
    (if-let [len (when (ty/vec-type? target-ty) (ty/vec-size target-ty))]
      (ast/->Literal len)
      (throw (ex-info "Cannot determine array length" {:node node})))))

(defmethod lower-expr :default [node _ctx]
  (throw (ex-info (str "Unsupported IR2 expression: " (n/kind node)) {:node node})))

;; ── 语句降级 ──
(defmulti lower-stmt (fn [node _ctx] (n/kind node)))

(defmethod lower-stmt :block [node ctx]
  (let [stmts (mapv #(lower-stmt % ctx) (n/block-exprs node))]
    (ast/->Block stmts)))

(defmethod lower-stmt :let [node ctx]
  ;; let 展开为 var-decl 序列 + body
  (let [bindings (n/let-bindings node)
        decls (mapv (fn [b]
                      (ast/->VarDecl (var-name (:var b))
                                     (ir-type (:var b))
                                     (lower-expr (:val b) ctx)))
                    bindings)
        [body-block ret-expr] (lower-body (n/let-body node) ctx)
        all-stmts (if ret-expr
                    (conj (vec decls) body-block ret-expr)
                    (into (vec decls) (if (= :block (ast/kind body-block)) (:stmts body-block) [body-block])))]
    (ast/->Block all-stmts)))

(defmethod lower-stmt :assign [node ctx]
  (ast/->Assign (lower-expr (n/assign-var node) ctx)
                (lower-expr (n/assign-val node) ctx)))

(defmethod lower-stmt :while [node ctx]
  (let [test (lower-expr (n/while-test node) ctx)
        body (lower-stmt (n/while-body node) ctx)
        body-block (if (= :block (ast/kind body)) body (ast/->Block [body]))]
    (ast/->While test body-block)))

(defmethod lower-stmt :if [node ctx]
  (let [test (lower-expr (n/if-test node) ctx)
        then (lower-stmt (n/if-then node) ctx)
        then-block (if (= :block (ast/kind then)) then (ast/->Block [then]))
        else-block (when-let [e (n/if-else node)]
                     (let [e (lower-stmt e ctx)]
                       (if (= :block (ast/kind e)) e (ast/->Block [e]))))]
    (ast/->If test then-block else-block)))

(defmethod lower-stmt :default [node ctx]
  ;; 默认作为表达式（语句位置允许表达式）
  (lower-expr node ctx))

;; ── 统一入口 ──
(defn lower-node [node ctx]
  (case (n/kind node)
    (:literal :variable :call :vector :member-access :convert
      :new-array :aget :aset :alength) (lower-expr node ctx)
    (:block :let :assign :while :if) (lower-stmt node ctx)
    ;; 未知节点尝试按顶层处理（可能出现在非顶层位置）
    (throw (ex-info (str "Unexpected IR2 node in expression/statement context: " (n/kind node)) {:node node}))))

(defn lower-nodes
  "将 IR2 根节点向量降级为 ShaderAST 声明列表。"
  [ir2-roots ctx]
  (let [ns-nodes (filter #(= :ns (n/kind %)) ir2-roots)
        imports (mapcat #(lower-top % ctx) ns-nodes)
        rest-nodes (remove #(= :ns (n/kind %)) ir2-roots)
        decls (mapcat #(lower-top % ctx) rest-nodes)]
    (into (vec imports) decls)))
(ns top.kzre.homunculus.backend.hlsl.emit
  "HLSL 代码发射器。通过 shader.core 获取通用数据，使用 hlsl.templates 生成语法。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
    [top.kzre.homunculus.backend.shader.core :as sc]
    [top.kzre.homunculus.backend.shader.types :as st]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.core.types.metadata :as md]))

;; ── 类型转换适配 ────────────────────────
(defn- hlsl-type-str [ir-type]
  (st/shader-type-str (ty/type-sym ir-type)))

;; ── 分发多方法 ──────────────────────────
(defmulti emit-node (fn [node] (n/kind node)))

;; ── 叶子 ────────────────────────────────
(defmethod emit-node :literal [node]
  (tmpl/hlsl-literal (n/lit-val node)))

(defmethod emit-node :variable [node]
  (tmpl/var-ref (name (n/var-name node))))

;; ── 调用 ────────────────────────────────
(defmethod emit-node :call [node]
  (let [fn-name (emit-node (n/call-fn node))
        args    (mapv emit-node (n/call-args node))]
    (tmpl/call fn-name (str/join ", " args))))

;; ── 控制流 ──────────────────────────────
(defmethod emit-node :if [node]
  (let [test (emit-node (n/if-test node))
        then (emit-node (n/if-then node))]
    (if-let [else (n/if-else node)]
      (tmpl/if-else-stmt test then (emit-node else))
      (tmpl/if-stmt test then))))

(defmethod emit-node :block [node]
  (str/join "\n" (mapv emit-node (n/block-exprs node))))

(defmethod emit-node :while [node]
  (tmpl/while-stmt (emit-node (n/while-test node))
                   (emit-node (n/while-body node))))

;; ── 赋值 ────────────────────────────────
(defmethod emit-node :assign [node]
  (tmpl/assign (emit-node (n/assign-var node))
               (emit-node (n/assign-val node))))

;; ── let 绑定 ────────────────────────────
(defmethod emit-node :let [node]
  (let [bindings (n/let-bindings node)
        decls    (mapv (fn [[v e]]
                         (tmpl/var-decl-init
                           (hlsl-type-str (ty/get-type v))
                           (name (n/var-name v))
                           (emit-node e)))
                       bindings)
        body     (emit-node (n/let-body node))]
    (str (str/join "\n" decls) "\n" body)))

;; ── 类型转换 ────────────────────────────
(defmethod emit-node :convert [node]
  (tmpl/type-cast (hlsl-type-str (n/convert-dst-ty node))
             (emit-node (n/convert-expr node))))

;; ── 成员访问 ────────────────────────────
(defmethod emit-node :member-access [node]
  (let [target (emit-node (n/access-target node))
        member (n/access-member node)]
    (tmpl/member-access target member)))

;; ── 向量构造器 ──────────────────────────
(defmethod emit-node :vector [node]
  (let [items    (n/vector-items node)
        emitted  (mapv emit-node items)
        vty      (ty/get-type node)
        ;; 取第一个元素类型或回退为 float
        elem-kw  (when (ty/hetero-vec? vty) (first (ty/hetero-vec-types vty)))
        type-str (if elem-kw (hlsl-type-str elem-kw) "float")]
    (str type-str "(" (str/join ", " emitted) ")")))

;; ── lambda（匿名） ──────────────────────
(defmethod emit-node :lambda [node]
  (emit-node (n/lambda-body node)))

;; ── 函数与全局常量 ──────────────────────
(defmethod emit-node :define [node]
  (let [val (n/define-val node)]
    (if (= (n/kind val) :lambda)
      ;; 普通函数定义
      (let [lam        val
            params     (n/lambda-params lam)
            ret-type   (hlsl-type-str (ty/fun-ret (ty/get-type lam)))
            param-strs (mapv (fn [p]
                               (str (hlsl-type-str (ty/get-type p))
                                    " " (name (n/var-name p))))
                             params)
            body       (emit-node (n/lambda-body lam))
            func-name  (name (n/define-name node))]
        (str (tmpl/func-signature ret-type func-name (str/join ", " param-strs))
             "\n"
             (tmpl/func-body body)))
      ;; 全局常量
      (tmpl/var-decl-init (hlsl-type-str (ty/get-type val))
                          (name (n/define-name node))
                          (emit-node val)))))

;; ── 资源声明（虚拟 kind） ──────────────
(defmethod emit-node :define-resource [node]
  (let [attrs    (n/attrs node)
        res-kind (:shader/resource-kind attrs)
        res-name (name (n/define-name node))
        val-node (n/define-val node)
        reg      (when val-node (n/lit-val val-node))]
    (case res-kind
      :texture2D (tmpl/texture2d-decl res-name reg)
      :sampler   (tmpl/sampler-decl res-name reg)
      :cbuffer   (let [members-str
                       (when val-node
                         (let [args (n/call-args val-node)]
                           (str/join "\n"
                                     (map (fn [m] (tmpl/struct-member
                                                    (hlsl-type-str (ty/get-type m))
                                                    (name (n/var-name m))
                                                    nil))
                                          args))))]
                   (tmpl/cbuffer-decl res-name reg members-str))
      (throw (ex-info "Unknown resource type" {:node node})))))

(defmethod emit-node :default [node]
  (throw (ex-info (str "HLSL emit not implemented for " (n/kind node)) {:node node})))

;; ── 入口包装（利用 shader.core 的 entry-spec） ──
(defn- emit-entry-wrapper [stage define-node]
  (let [{:keys [input-params output-params]} (sc/entry-spec stage define-node hlsl-type-str)
        func-name (name (n/define-name define-node))
        input-struct-name (str func-name "_Input")
        output-struct-name (str func-name "_Output")
        ;; 结构体成员
        input-members  (str/join "\n" (mapv (fn [p] (tmpl/struct-member (:type p) (:name p) (:semantic p))) input-params))
        output-members (str/join "\n" (mapv (fn [p] (tmpl/struct-member (:type p) (:name p) (:semantic p))) output-params))
        input-struct  (tmpl/struct-decl input-struct-name input-members)
        output-struct (tmpl/struct-decl output-struct-name output-members)
        ;; 调用核心函数
        call-args    (str/join ", " (mapv (fn [p] (str "in." (:name p))) input-params))
        core-call    (str func-name "(" call-args ")")
        wrapper-body (if (= stage :vertex)
                       (str "VSOutput out;\n"
                            "out.position = " core-call ";\n"
                            "return out;")
                       (str "return " core-call ";"))]
    (str input-struct "\n"
         output-struct "\n"
         (tmpl/entry-wrapper stage func-name input-struct-name output-struct-name wrapper-body))))

;; ── 公共入口 ────────────────────────────
(defn emit
  "对 IR2 根节点列表发射 HLSL 代码。"
  [ir2-roots]
  (let [flat      (mapcat n/unwrap-body ir2-roots)
        defines   (filter n/define-node? flat)
        {:keys [resources globals functions]} (sc/classify-defines defines)
        resource-strs  (mapv (fn [d] (emit-node (assoc d :kind :define-resource))) resources)
        global-strs    (mapv emit-node globals)
        fn-strs        (mapv emit-node functions)
        entry-fns      (filter #(md/fn-shader-stage %) functions)
        entry-wrappers (mapv (fn [d] (emit-entry-wrapper (md/fn-shader-stage d) d)) entry-fns)]
    (str/join "\n\n" (concat resource-strs global-strs fn-strs entry-wrappers))))
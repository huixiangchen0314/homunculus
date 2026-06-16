;; top.kzre.homunculus.backend.unity.emit
(ns top.kzre.homunculus.backend.unity.emit
  "Unity ShaderLab 代码发射器。利用 shader.core 与 hlsl.templates 实现完整代码生成。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.unity.templates :as t]
    [top.kzre.homunculus.backend.hlsl.templates :as hlsl]
    [top.kzre.homunculus.backend.shader.core :as sc]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.core.types.metadata :as md]))

;; ── 类型转换适配 ────────────────────────
(defn- unity-type-str [ir-type]
  (t/unity-type-str ir-type))

;; ── HLSL 节点发射（复用 hlsl.templates 语法） ──
(defmulti emit-node (fn [node] (n/kind node)))

(defmethod emit-node :literal [node]
  (hlsl/hlsl-literal (n/lit-val node)))
(defmethod emit-node :variable [node]
  (name (n/var-name node)))
(defmethod emit-node :call [node]
  (let [fn-name (emit-node (n/call-fn node))
        args    (mapv emit-node (n/call-args node))]
    (hlsl/call fn-name (str/join ", " args))))
(defmethod emit-node :if [node]
  (let [test (emit-node (n/if-test node))
        then (emit-node (n/if-then node))]
    (if-let [else (n/if-else node)]
      (hlsl/if-else-stmt test then (emit-node else))
      (hlsl/if-stmt test then))))
(defmethod emit-node :block [node]
  (str/join "\n" (mapv emit-node (n/block-exprs node))))
(defmethod emit-node :while [node]
  (hlsl/while-stmt (emit-node (n/while-test node))
                   (emit-node (n/while-body node))))
(defmethod emit-node :assign [node]
  (hlsl/assign (emit-node (n/assign-var node))
               (emit-node (n/assign-val node))))
(defmethod emit-node :let [node]
  (let [bindings (n/let-bindings node)
        decls    (mapv (fn [[v e]]
                         (hlsl/var-decl-init
                           (unity-type-str (ty/get-type v))
                           (name (n/var-name v))
                           (emit-node e)))
                       bindings)
        body     (emit-node (n/let-body node))]
    (str (str/join "\n" decls) "\n" body)))
(defmethod emit-node :convert [node]
  (hlsl/type-cast (unity-type-str (n/convert-dst-ty node))
             (emit-node (n/convert-expr node))))
(defmethod emit-node :member-access [node]
  (let [target (emit-node (n/access-target node))
        member (n/access-member node)]
    (hlsl/member-access target member)))
(defmethod emit-node :vector [node]
  (let [items    (n/vector-items node)
        emitted  (mapv emit-node items)
        vty      (ty/get-type node)
        elem-kw  (when (ty/hetero-vec? vty) (first (ty/hetero-vec-types vty)))
        type-str (if elem-kw (unity-type-str elem-kw) "float")]
    (str type-str "(" (str/join ", " emitted) ")")))
(defmethod emit-node :lambda [node]
  (emit-node (n/lambda-body node)))
(defmethod emit-node :define [node]
  (let [val (n/define-val node)]
    (if (= (n/kind val) :lambda)
      (let [lam        val
            params     (n/lambda-params lam)
            ret-type   (unity-type-str (ty/get-type lam))
            param-strs (mapv (fn [p]
                               (str (unity-type-str (ty/get-type p))
                                    " " (name (n/var-name p))))
                             params)
            body       (emit-node (n/lambda-body lam))
            func-name  (name (n/define-name node))]
        (str (hlsl/func-signature ret-type func-name (str/join ", " param-strs))
             "\n"
             (hlsl/func-body body)))
      ;; 全局常量
      (hlsl/var-decl-init (unity-type-str (ty/get-type val))
                          (name (n/define-name node))
                          (emit-node val)))))

(defmethod emit-node :default [node]
  (throw (ex-info (str "Unity emit not implemented for " (n/kind node)) {:node node})))

;; ── 资源属性 ────────────────────────────
(defn- resource->property [res-define]
  (let [attrs    (n/attrs res-define)
        res-kind (:shader/resource-kind attrs)
        name     (name (n/define-name res-define))]
    (case res-kind
      :texture2D (t/property-2d name name "white")
      :sampler   (t/property-float name name 0)
      :cbuffer   nil
      nil)))

;; ── 全局常量声明（Uniform） ──────────────
(defn- emit-global-uniform [d]
  (let [val (n/define-val d)
        ty (ty/get-type val)]
    (str "uniform " (unity-type-str ty) " " (name (n/define-name d)) ";")))

;; ── 入口包装：生成 VS/PS 结构体和 main 函数 ──
(defn- emit-entry-wrapper [stage define-node]
  (let [{:keys [input-params output-params]} (sc/entry-spec stage define-node unity-type-str)
        entry-fn-name (name (n/define-name define-node))
        input-struct-name (str entry-fn-name "_Input")
        output-struct-name (str entry-fn-name "_Output")
        ;; 生成结构体
        input-members (str/join "\n" (mapv (fn [p] (hlsl/struct-member (:type p) (:name p) (:semantic p))) input-params))
        output-members (str/join "\n" (mapv (fn [p] (hlsl/struct-member (:type p) (:name p) (:semantic p))) output-params))
        input-struct  (hlsl/struct-decl input-struct-name input-members)
        output-struct (hlsl/struct-decl output-struct-name output-members)
        ;; 核心函数引用
        call-args (str/join ", " (mapv (fn [p] (str "in." (:name p))) input-params))
        core-call (str entry-fn-name "(" call-args ")")
        ;; 包装函数
        vert-wrapper (str "VSOutput vert(VSInput in) {\n"
                          "    VSOutput out;\n"
                          "    out.position = " core-call ";\n"
                          "    return out;\n"
                          "}")
        frag-wrapper (str "float4 frag(VSOutput in) : SV_TARGET {\n"
                          "    return " core-call ";\n"
                          "}")
        wrapper (case stage
                  :vertex   vert-wrapper
                  :fragment frag-wrapper
                  (throw (ex-info "Unsupported stage" {:stage stage})))]
    (str input-struct "\n" output-struct "\n" wrapper)))

;; ── 生成 Pass 内容 ────────────────────────
(defn- emit-pass [entry]
  (let [stage        (md/fn-shader-stage entry)
        vert-name    (str (name (n/define-name entry)) "_vert")
        frag-name    (str (name (n/define-name entry)) "_frag")
        entry-wrapper (emit-entry-wrapper stage entry)
        ;; 收集此 pass 需要的全局常量和辅助函数
        ;; （实际应该从完整的 ir2-roots 中获取，这里为简化，假设 emit 顶层已收集）
        ;; 这里只添加核心函数和包装器，全局和辅助由外层拼接
        hlsl-body (str/join "\n" [(emit-node entry) entry-wrapper])
        pass-tags  "Tags { \"RenderType\"=\"Opaque\" }"]
    (t/pass (name (n/define-name entry))
            pass-tags
            (str (t/pragma-vertex vert-name) "\n"
                 (t/pragma-fragment frag-name) "\n"
                 (t/include-unity-cg) "\n"
                 (t/hlsl-program hlsl-body)))))

;; ── 公共入口 ────────────────────────────
(defn emit [ir2-roots]
  (let [flat      (mapcat n/unwrap-body ir2-roots)
        defines   (filter n/define-node? flat)
        {:keys [resources globals functions]} (sc/classify-defines defines)
        ;; Properties
        prop-lines     (keep resource->property resources)
        properties-str (when (seq prop-lines) (apply t/properties prop-lines))
        ;; 全局常量（uniform）
        global-strs    (mapv emit-global-uniform globals)
        ;; 辅助函数（非入口）
        non-entry-fns  (remove #(md/fn-shader-stage %) functions)
        entry-fns      (filter #(md/fn-shader-stage %) functions)
        helper-fn-strs (mapv emit-node non-entry-fns)
        ;; 每个入口生成一个 Pass
        passes         (mapv emit-pass entry-fns)
        ;; 组装 SubShader
        subshader-body (str/join "\n" (concat
                                        (when (seq global-strs)
                                          [(str "// Globals\n" (str/join "\n" global-strs))])
                                        (when (seq helper-fn-strs)
                                          [(str/join "\n" helper-fn-strs)])
                                        passes))
        subshader      (t/subshader "Tags { \"RenderType\"=\"Opaque\" }" subshader-body)
        shader-body    (str (when properties-str (str properties-str "\n")) subshader)
        shader-code    (t/shader "CustomShader" shader-body)]
    shader-code))
(ns top.kzre.homunculus.backend.hlsl.core
  "HLSL 代码发射核心：多方法分派与公共辅助。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.templates :as tmpl]
    [top.kzre.homunculus.backend.shader.core :as sc]
    [top.kzre.homunculus.backend.shader.types :as st]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.type :as ty]
    [top.kzre.homunculus.core.types.metadata :as md]))

;; 核心工具函数 ==============================================



;; ── 类型转换适配 ──
(defn hlsl-type-str [ir-type]
  (st/shader-type-str (ty/type-sym ir-type)))

;; ── 多方法分发 ──
(defmulti emit-node (fn [node]
                      (n/kind node)))

(defmethod emit-node :default [node]
  (throw (ex-info (str "HLSL emit not implemented for " (n/kind node)) {:node node})))

;; ── 入口包装（使用 shader.core）──
(defn- emit-entry-wrapper [stage define-node]
  (let [{:keys [input-params output-params]} (sc/entry-spec stage define-node hlsl-type-str)
        func-name (name (n/define-name define-node))
        input-struct-name  (str func-name "_Input")
        output-struct-name (str func-name "_Output")

        ;; ── 顶点着色器自动输出非 POSITION 的插值语义 ──
        output-params (if (= stage :vertex)
                        (let [passthrough (mapv (fn [in]
                                                  {:name     (:name in)
                                                   :type     (:type in)
                                                   :semantic (:semantic in)})
                                                (filter #(and (:semantic %)
                                                              (not= "POSITION" (:semantic %)))
                                                        input-params))]
                          (concat passthrough output-params))
                        output-params)

        ;; 返回类型：顶点用输出结构体名，片段用唯一输出成员的类型（如 float4）
        return-type (if (= stage :vertex)
                      output-struct-name
                      (-> output-params first :type))

        input-members  (str/join "\n" (mapv (fn [p] (tmpl/struct-member (:type p) (:name p) (:semantic p))) input-params))
        output-members (str/join "\n" (mapv (fn [p] (tmpl/struct-member (:type p) (:name p) (:semantic p))) output-params))

        input-struct  (tmpl/struct-decl input-struct-name input-members)
        output-struct (tmpl/struct-decl output-struct-name output-members)

        call-args (str/join ", " (mapv (fn [p] (str "input." (:name p))) input-params))
        core-call (str func-name "(" call-args ")")

        wrapper-body (if (= stage :vertex)
                       (let [out-assigns (str/join "\n"
                                                   (for [p output-params]
                                                     (if (= "SV_POSITION" (:semantic p))
                                                       (str "out." (:name p) " = " core-call ";")
                                                       (str "out." (:name p) " = input." (:name p) ";"))))]
                         (str output-struct-name " out;\n" out-assigns "\nreturn out;"))
                       (str "return " core-call ";"))]
    (str input-struct "\n"
         output-struct "\n"
         (tmpl/entry-wrapper stage func-name
                             input-struct-name output-struct-name
                             return-type
                             wrapper-body))))


(defn emit-resource-decl [node]
  (let [attrs    (n/node-meta node)
        res-kind (:shader/resource-kind attrs)
        res-name (name (n/define-name node))
        ;; 从元数据中提取寄存器信息
        reg      (case res-kind
                   :texture2D (:shader/texture-register attrs)
                   :sampler   (:shader/sampler-register attrs)
                   :cbuffer   (:shader/cbuffer-register attrs)
                   nil)]
    (case res-kind
      :texture2D (tmpl/texture2d-decl res-name (name reg))
      :sampler   (tmpl/sampler-decl res-name (name reg))
      :cbuffer   (let [members (:shader/cbuffer-members attrs)
                       member-strs (map (fn [[sym type-sym-node]] (tmpl/struct-member (st/shader-type-str (keyword type-sym-node)) (name sym) nil)) members)]
                   (tmpl/cbuffer-decl res-name (name reg) (clojure.string/join "\n" member-strs)))
      (throw (ex-info "Unknown resource type" {:node node})))))


;; 在 hlsl/core.clj 中添加
(defn emit-uniform-decl [node]
  (let [val (n/define-val node)
        type-str (hlsl-type-str (ty/get-type val))
        name-str (name (n/define-name node))]
    (tmpl/uniform-var-decl type-str name-str)))

(defn emit-static-var-decl [node]
  (let [ir-type (ty/get-type node)
        type-str (hlsl-type-str ir-type)
        name-str (name (n/define-name node))
        val-expr (or (n/define-val node)
                                (throw (ex-info "Static variable must have an initializer" {:name name-str})))
        val-expr-str (emit-node val-expr)]
    (tmpl/static-var-decl-init type-str name-str val-expr-str)))


;; ── 公共入口 ──
(defn emit
  "对 IR2 根节点列表发射 HLSL 代码。"
  [ir2-roots]
  (let [flat      (mapcat n/unwrap-body ir2-roots)
        defines   (filter n/define-node? flat)
        records   (filter n/record-node? flat)
        {:keys [resources uniforms static-vars global-vars functions]} (sc/classify-defines defines)
        resource-strs  (mapv (fn [d] (emit-resource-decl d )) resources)
        uniform-strs  (mapv emit-uniform-decl uniforms)
        static-var-strs (mapv emit-static-var-decl static-vars)
        global-var-strs    (mapv emit-node global-vars)
        struct-strs       (mapv emit-node records)
        fn-strs        (mapv emit-node functions)
        entry-fns      (filter #(md/fn-shader-stage %) functions)
        entry-wrappers (mapv (fn [d] (emit-entry-wrapper (md/fn-shader-stage d) d)) entry-fns)]
    (str/join "\n\n" (concat resource-strs uniform-strs static-var-strs global-var-strs struct-strs fn-strs entry-wrappers))))
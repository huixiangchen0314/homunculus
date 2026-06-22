(ns top.kzre.homunculus.backend.hlsl.core
  "HLSL 代码发射核心：多方法分派与公共辅助。"
  (:require
    [clojure.string :as str]
    [top.kzre.homunculus.backend.hlsl.render :as render]
    [top.kzre.homunculus.backend.shader.core :as sc]
    [top.kzre.homunculus.backend.shader.types :as st]
    [top.kzre.homunculus.core.ir2.node :as n]
    [top.kzre.homunculus.core.types.metadata :as md]
    [top.kzre.homunculus.core.types.type :as ty]))

(defn hlsl-type-str [ir-type]
  (cond
    (ty/vec-type? ir-type)                                  ;; 同构数组类型
    (let [vec-size (ty/vec-size ir-type)]
      (str (hlsl-type-str (ty/vec-element-type ir-type)) "[" vec-size "]"))
    :else (st/shader-type-str (ty/type-sym ir-type))))

(defmulti emit-node
          "将 IR2 节点转换为 HLSL AST 节点（带标签的向量或字符串）。

          功能：
          根据 IR 节点的种类（kind）分派到对应的实现方法，产出规范的 HLSL AST 节点。
          AST 节点可以是字符串（用于纯文本直出）或形如 [:tag ...] 的向量，标签由 render.clj 中的
          multimethod 识别并渲染为 HLSL 源代码。

          参数：
          - node    : IR2 节点（来自 top.kzre.homunculus.core.ir2.node）
          - context : 编译上下文，可能包含已知类型、命名空间、前端/后端等信息。
                     具体结构由调用方决定，但至少应包含 :known-types 映射。

          返回值规格：
          返回一个符合 top.kzre.homunculus.backend.hlsl.spec 规范的 ::node 值，
          即以下之一：
          - 字符串（用于 #include 指令、直接嵌入的原始文本等）
          - 带标签的向量，标签包括：
            :literal :var-ref :call :sample :binary :member-access :constructor :cast
            :return :var-decl :assign :expr-stmt :if :while :block
            :function :entry-wrapper :import :struct-member :struct
            :texture-decl :sampler-decl :cbuffer-decl :uniform-decl :static-var-decl
            :comment :new-array :aget :aset :alength :raw
            等（详见 spec.clj 中的 node-tag 多方法定义）。

          注意：
          - 如果遇到无法处理的 IR 节点种类，默认实现应抛出异常。
          - 该多方法不直接产生字符串输出，而是构建 AST，由 render 模块最终渲染。"
          (fn [node _context] (n/kind node)))

(defmethod emit-node :default [node _context]
  (throw (ex-info (str "HLSL emit not implemented for " (n/kind node)) {:node node})))

;; ── 入口包装 ──
(defn- emit-entry-wrapper [define-node]
  (let [stage (md/fn-shader-stage define-node)
        {:keys [input-params output-params]} (sc/entry-spec stage define-node hlsl-type-str)
        func-name (name (n/define-name define-node))
        input-struct-name  (str func-name "_Input")
        output-struct-name (str func-name "_Output")

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

        ;; 成员必须用 :struct-member 标签
        input-members  (mapv (fn [p] [:struct-member (:type p) (:name p) (:semantic p)]) input-params)
        output-members (mapv (fn [p] [:struct-member (:type p) (:name p) (:semantic p)]) output-params)

        input-struct  [:struct input-struct-name input-members]
        output-struct [:struct output-struct-name output-members]

        call-args (str/join ", " (mapv (fn [p] (str "input." (:name p))) input-params))
        core-call (str func-name "(" call-args ")")

        ;; 包装体：顶点着色器需要声明输出变量并赋值
        body-stmts (if (= stage :vertex)
                     (let [out-decl [:raw (str output-struct-name " out;")]
                           assignments (mapv (fn [p]
                                               (if (= "SV_POSITION" (:semantic p))
                                                 [:assign (str "out." (:name p)) [:literal core-call]]
                                                 [:assign (str "out." (:name p)) [:member-access [:var-ref "input"] (:name p)]]))
                                             output-params)
                           return-stmt [:return [:var-ref "out"]]]
                       (into [out-decl] (conj assignments return-stmt)))
                     [[:return [:literal core-call]]])

        wrapper-fn (if (= stage :vertex)
                     [:entry-wrapper :vertex func-name input-struct-name output-struct-name output-struct-name
                      body-stmts]
                     [:entry-wrapper :fragment func-name input-struct-name output-struct-name
                      (-> output-params first :type)
                      body-stmts])]
    [input-struct output-struct wrapper-fn]))

;; ── 资源声明 ──
(defn emit-resource-decl [node]
  (let [attrs    (n/node-meta node)
        res-kind (:shader/resource-kind attrs)
        res-name (name (n/define-name node))
        reg      (case res-kind
                   :texture2D (:shader/texture-register attrs)
                   :sampler   (:shader/sampler-register attrs)
                   :cbuffer   (:shader/cbuffer-register attrs)
                   nil)]
    (case res-kind
      :texture2D [:texture-decl res-name (name reg)]
      :sampler   [:sampler-decl res-name (name reg)]
      :cbuffer   (let [members (:shader/cbuffer-members attrs)
                       member-vecs (mapv (fn [[sym type-sym-node]]
                                          [:struct-member (st/shader-type-str (keyword type-sym-node)) (name sym) nil])
                                        members)]
                   [:cbuffer-decl res-name (name reg) member-vecs])
      (throw (ex-info "Unknown resource type" {:node node})))))

(defn emit-uniform-decl [node context]
  (let [val (n/define-val node)
        type-str (hlsl-type-str (ty/get-type val))
        name-str (name (n/define-name node))]
    [:uniform-decl type-str name-str]))

(defn emit-static-var-decl [node context]
  (let [ir-type (ty/get-type node)
        type-str (hlsl-type-str ir-type)
        name-str (name (n/define-name node))
        val-expr (or (n/define-val node)
                     (throw (ex-info "Static variable must have an initializer" {:name name-str})))
        init-struct (emit-node val-expr context)]
    [:static-var-decl type-str name-str init-struct]))

;; ── 全局入口 ──
(defn emit
  [ir2-roots context]
  (let [ns-node   (first (filter n/ns-node? ir2-roots))
        flat      (mapcat n/unwrap-body ir2-roots)
        defines   (filter n/define-node? flat)
        records   (filter n/record-node? flat)
        {:keys [resources uniforms static-vars global-vars functions]} (sc/classify-defines defines)
        import-structs         (when ns-node (emit-node ns-node context)) ;; 多个import 语句
        resource-structs  (mapv emit-resource-decl resources)
        uniform-structs   (mapv #(emit-uniform-decl % context) uniforms)
        static-var-structs (mapv #(emit-static-var-decl % context) static-vars)
        global-var-structs (mapv #(emit-node % context) global-vars)
        struct-structs     (mapv #(emit-node % context) records)
        fn-structs         (mapv #(emit-node % context) functions)
        entry-fns          (filter #(md/fn-shader-stage %) functions)
        entry-wrapper-structs (mapcat emit-entry-wrapper entry-fns)
        all-structs (remove nil? (concat  import-structs
                                         resource-structs
                                         uniform-structs
                                         static-var-structs
                                         global-var-structs
                                         struct-structs
                                         fn-structs
                                         entry-wrapper-structs))]
    (render/render (vec all-structs))))
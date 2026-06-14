(ns top.kzre.homunculus.backend.shader.emit
  "着色器通用代码生成器，基于 IR2 节点种类和 IShaderBackend 协议。

   本文件包含：
   1. `emit` 多方法 —— 将 IR2 节点转换为目标语言字符串
   2. `generate` 主函数 —— 编排整个着色器程序的生成流程

   generate 的流水线如下：
   IR2 根节点列表
     → 展平顶层 :block 节点
     → 过滤出 :define 节点
     → 分类为资源、全局常量、函数定义
     → 发射全局声明（资源 + 全局常量）
     → 构建 entry-specs（自动推导参数或使用手动指定）
     → 装配最终程序并调用后端 shader-program"
  (:require [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.string :as str]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]))

;; ═══════════════════════════════════════════════════════
;; emit 多方法 —— 单个 IR2 节点的代码生成
;; ═══════════════════════════════════════════════════════

(defmulti emit
          "将 IR2 节点转换为目标着色器语言的代码字符串。
           分派依据：
           - 如果 node 实现了 INode 协议，使用其 :kind
           - 如果 node 是 map（例如 check pass 插入的 :convert 节点），使用 (:kind node)
           - 否则抛出异常"
          (fn [node backend]
            (cond
              (satisfies? ir2p/INode node) (ir2p/kind node)
              (map? node)                   (:kind node)
              :else                         (throw (ex-info "Unsupported node type" {:node node})))))

(defmethod emit :default [node backend]
  (throw (ex-info (str "Unhandled node: " node) {:node node})))

;; ═══════════════════════════════════════════════════════
;; generate 的辅助函数
;; ═══════════════════════════════════════════════════════

(defn- flatten-roots
  "步骤 1：展平顶层 :block 节点。

   输入：ir2-roots —— 一个 IR2 根节点序列，可能包含 :block 节点（由 do 形式产生）。
   输出：一个展平后的节点序列，其中所有 :block 节点的子表达式被提取到顶层。

   例如：
   输入 [(:block {:exprs [define1 define2]}) (:literal {...})]
   输出 [define1 define2 (:literal {...})]"
  [ir2-roots]
  (mapcat (fn [r]
            (if (= (ir2p/kind r) :block)
              (:exprs r)
              [r]))
          ir2-roots))

(defn- resource-type?
  "判断一个 IType 是否为资源类型（:texture2D, :sampler, :cbuffer）。

   输入：ty —— 一个 IType 实例（可能为 nil）
   输出：布尔值"
  [ty]
  (and ty
       (= (tp/type-kind ty) :con)
       (contains? #{:texture2D :sampler :cbuffer} (:name ty))))

(defn- classify-defines
  "步骤 2：将 :define 节点分类为资源、全局常量和函数定义。

   输入：defines —— 一个 :define 节点列表
   输出：一个 map，包含三个键：
         :resources —— 资源声明节点（纹理、采样器、cbuffer）
         :globals   —— 全局常量节点（非资源、非 lambda、无 shader-stage）
         :functions —— 函数定义节点（val 为 lambda）

   分类规则：
   - 资源：val 的类型是 :texture2D, :sampler 或 :cbuffer
   - 全局常量：val 不是 lambda，且元数据中没有 :shader-stage
   - 函数：val 是 lambda"
  [defines]
  (let [is-resource? (fn [d] (resource-type? (get-in d [:val :attrs :type])))
        is-global?   (fn [d]
                       (and (= (ir2p/kind d) :define)
                            (not (is-resource? d))
                            (not (some? (some-> (ir2p/node-meta d) :shader-stage)))
                            (let [v (:val d)]
                              (not (and v (= (ir2p/kind v) :lambda))))))
        is-function? (fn [d]
                       (and (= (ir2p/kind d) :define)
                            (not (is-resource? d))
                            (let [v (:val d)]
                              (and v (= (ir2p/kind v) :lambda)))))]
    {:resources (filter is-resource? defines)
     :globals   (filter is-global? defines)
     :functions (filter is-function? defines)}))

(defn- extract-params
  "从函数参数列表中提取元信息，用于构建 entry-spec。

   输入：
     backend —— 着色器后端实例
     params  —— IR2 参数节点列表（VariableNode）
   输出：一个向量，每个元素是一个 map，包含：
         :name     —— 安全处理后的变量名
         :type     —— 类型字符串
         :semantic —— 语义字符串（如 \"SV_POSITION\"），无则为 nil

   参数的类型从节点的 :attrs :type 获取，语义从节点的元数据中查找首字母大写的关键字。"
  [backend params]
  (mapv (fn [p]
          {:name     (sp/shader-var-ref backend (:name p))
           :type     (if-let [ty (get-in p [:attrs :type])]
                       (sp/shader-type backend ty)
                       "float")
           :semantic (some (fn [k]
                             (when (and (keyword? k)
                                        (not (namespace k))
                                        (re-find #"^[A-Z]" (name k)))
                               (name k)))
                           (keys (ir2p/node-meta p)))})
        params))

(defn- emit-globals
  "步骤 4：发射所有全局声明（资源 + 全局常量）。

   输入：
     resources —— 资源 define 节点列表
     globals   —— 全局常量 define 节点列表
     backend   —— 着色器后端实例
   输出：一个字符串序列，包含资源声明和全局常量声明。

   资源声明通过 emit 方法生成；全局常量调用 shader-global-decl 协议方法生成。"
  [resources globals backend]
  (concat (map #(emit % backend) resources)
          (for [d globals
                :let [val (:val d)
                      ir-type (get-in val [:attrs :type])
                      init-expr (when (and val (not (= (ir2p/kind val) :lambda)))
                                  (emit val backend))]]
            (sp/shader-global-decl backend (name (:name d)) ir-type init-expr))))

(defn- auto-entry-spec
  "为已有函数定义自动构建 entry-spec。

   输入：
     stage       —— 着色器阶段关键字（:vertex, :fragment 等）
     existing-d  —— 一个 :define 节点，其 val 为 LambdaNode
     backend     —— 着色器后端实例
   输出：一个 map，包含 :input-params 和 :output-params 向量。

   片段着色器的输入参数仅保留带有语义的参数（如 SV_POSITION）；其他阶段保留所有参数。
   输出参数根据函数体推断的类型生成，默认名称为 \"pos\"，语义根据阶段设置（:vertex → SV_POSITION，:fragment → SV_TARGET 等）。"
  [stage existing-d backend]
  (let [val          (:val existing-d)
        params       (:params val)
        raw-inputs   (if params (extract-params backend params) [])
        input-params (if (= stage :fragment)
                       (filterv (comp some? :semantic) raw-inputs)
                       raw-inputs)
        output-param (when val
                       (when-let [ret-ty (get-in (:body val) [:attrs :type])]
                         {:name     "pos"
                          :type     (sp/shader-type backend ret-ty)
                          :semantic (case stage
                                      :vertex   "SV_POSITION"
                                      :fragment "SV_TARGET"
                                      :geometry "SV_POSITION"
                                      nil)}))]
    {:input-params  input-params
     :output-params (if output-param [output-param] [])}))

(defn- temp-entry-spec
  "为无函数定义的情况创建临时入口规格。

   当用户直接提供表达式片段（如字面量）而没有 defshader 时，
   本函数生成一个临时函数，其函数体为 others 中的表达式。

   输入：
     stage   —— 着色器阶段
     fn-name —— 入口函数名
     others  —— 非 :define 的 IR2 表达式列表
     backend —— 着色器后端实例
   输出：一个 map，包含 :input-params, :output-params 和 :temp-fn（临时函数定义字符串）"
  [stage fn-name others backend]
  (let [body-code (when (seq others)
                    (let [emitted   (map #(emit % backend) others)
                          last-idx  (dec (count emitted))
                          emitted'  (map-indexed (fn [i code]
                                                   (if (= i last-idx)
                                                     (sp/shader-return backend code)
                                                     code))
                                                 emitted)]
                      (sp/shader-block backend emitted')))
        ret-type  (case stage :fragment "float4" "void")
        tmp-fn    (sp/shader-function-decl backend fn-name [] ret-type (or body-code ""))]
    {:input-params  []
     :output-params []
     :temp-fn       tmp-fn}))

(defn- build-entry-specs
  "步骤 5：构建所有入口的 entry-specs 列表。

   输入：
     entries       —— 用户提供的入口描述列表（可能为空）
     function-defs —— 已分类的函数定义节点列表
     others        —— 非 :define 的表达式列表
     backend       —— 着色器后端实例
   输出：entry-spec 列表，每个元素是一个 map，包含：
         :stage          —— 着色器阶段
         :fn-name        —— 安全处理后的函数名
         :input-params   —— 输入参数向量
         :output-params  —— 输出参数向量
         可能还有 :temp-fn —— 临时函数定义字符串（仅当无对应函数定义时）

   逻辑：
   1. 如果用户提供了 entries（非空），则使用它们；否则从函数定义的元数据中自动推导。
   2. 对于每个 entry，如果手动提供了 input-params 或 output-params，则直接使用。
   3. 否则，如果存在对应的函数定义，调用 auto-entry-spec 自动生成。
   4. 如果既无手动参数也无函数定义，调用 temp-entry-spec 创建临时函数。"
  [entries function-defs others backend]
  (let [fn-def-map (into {} (map (fn [d] [(name (:name d)) (emit d backend)]) function-defs))
        entries'   (if (seq entries)
                     entries
                     (for [d function-defs
                           :let [stage (some-> (ir2p/node-meta d) :shader-stage)
                                 fn-name (:name d)]
                           :when stage]
                       {:stage stage :fn-name (name fn-name)}))]
    (for [entry entries'
          :let [stage   (:stage entry)
                fn-name (:fn-name entry)
                safe-fn (sp/shader-var-ref backend fn-name)
                existing-d (some #(when (= (name (:name %)) fn-name) %) function-defs)]]
      (merge {:stage   stage
              :fn-name safe-fn}
             (if (or (:input-params entry) (:output-params entry))
               {:input-params  (or (:input-params entry) [])
                :output-params (or (:output-params entry) [])}
               (if existing-d
                 (auto-entry-spec stage existing-d backend)
                 (temp-entry-spec stage fn-name others backend)))))))

(defn- assemble-program
  "步骤 6：装配最终程序并调用后端 shader-program。

   输入：
     entry-specs —— 入口规格列表（可能包含 :temp-fn 字段）
     fn-def-map  —— 函数定义名到发射字符串的映射
     backend     —— 着色器后端实例
     globals     —— 全局声明字符串列表
   输出：完整的着色器程序字符串。

   将临时函数定义合并到最终函数列表中，并移除 entry-specs 中的 :temp-fn 字段。"
  [entry-specs fn-def-map backend globals]
  (let [final-fn-defs (concat (vals fn-def-map)
                              (keep :temp-fn entry-specs))
        final-specs   (mapv #(dissoc % :temp-fn) entry-specs)]
    (sp/shader-program backend final-fn-defs [] globals final-specs)))

;; ═══════════════════════════════════════════════════════
;; 主函数
;; ═══════════════════════════════════════════════════════

(defn generate
  "生成完整着色器代码。

   整个流水线：
   1. 展平顶层 :block 节点（处理 do 形式）。
   2. 过滤出 :define 节点，并按类型分类：
      - 资源（纹理、采样器、cbuffer）
      - 全局常量（普通 uniform）
      - 函数定义（lambda）
   3. 发射所有全局声明（资源 + 全局常量）。
   4. 构建 entry-specs 列表：
      - 如果用户提供了 entries，直接使用；否则从函数元数据推导。
      - 每个 entry-spec 包含阶段、函数名、输入/输出参数等信息。
   5. 装配最终程序：合并所有函数定义和入口包装，调用后端的 shader-program。

   参数：
     ir2-roots —— 经过所有 Pass 的 IR2 根节点列表
     backend   —— 实现了 IShaderBackend 协议的后端实例
     entries   —— 入口描述列表，每个元素为 {:stage :vertex/:fragment, :fn-name \"...\"}
                 可为空，此时从函数定义的 :shader-stage 元数据自动推导
   返回值：
     完整的着色器程序字符串"
  [ir2-roots backend entries]
  (let [flat-roots    (flatten-roots ir2-roots)
        defines       (filter #(= (ir2p/kind %) :define) flat-roots)
        {:keys [resources globals functions]} (classify-defines defines)
        fn-def-map    (into {} (map (fn [d] [(name (:name d)) (emit d backend)]) functions))
        globals       (emit-globals resources globals backend)
        others        (remove #(= (ir2p/kind %) :define) flat-roots)
        entry-specs   (build-entry-specs entries functions others backend)]
    (assemble-program entry-specs fn-def-map backend globals)))
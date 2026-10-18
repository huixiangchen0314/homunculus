(ns top.kzre.homunculus.backend.shader.pass.analyze.core
  "分析 pass。单遍遍历 shader AST，累积 SLAnalysis 所需信息。

   本 pass 只收集「涉及语义重分配」的信息：
     ▸ 结构降级——texture + sampler 的配对
     ▸ 语义降级——入口点的 params / return 展开

   遍历策略：每个节点接收 [node env]，返回 [node' env']。
   节点本身不被修改（node' == node）——本 pass 只累积 env。
   默认方法递归子节点，特定节点类型覆盖默认行为。

   语义相关字段通过 semantic 命名空间的辅助函数读取——
   不硬编码 meta / attrs 路径。"
  (:require
   [top.kzre.homunculus.backend.hlsl.lang :as lang]
   [top.kzre.homunculus.backend.shader.pass.analyze.env :as env]
   [top.kzre.homunculus.backend.shader.semantic :as semantic]
   [top.kzre.homunculus.backend.shader.ast :as ast]))


;; ═══════════════════════════════════════════
;; 辅助
;; ═══════════════════════════════════════════

(defn- var-name
  "从变量引用节点提取名字。非变量节点返回 nil。"
  [node]
  (when (and node (= :variable (ast/kind node)))
    (:name node)))

(defn- param->triple
  "HLSL AST 参数节点 → [name type semantic] 三元组。
   语义通过 semantic/shader-semantic 读取。"
  [p]
  [(:name p)
   (:type p)
   (semantic/shader-semantic p)])

;; ═══════════════════════════════════════════
;; 多方法分派
;; ═══════════════════════════════════════════

(defmulti analyze-node*
          "分析单个节点。返回 [node' env']。
           node' 通常等于 node——本 pass 不修改 AST。"
          (fn [node _env] (ast/kind node)))

;; ── 默认：递归子节点 ────────────────────────

(defmethod analyze-node* :default [node env]
  (ast/reduce-children node
                       (fn [child env] (analyze-node* child env))
                       env))

;; ── 纹理与采样器声明 ────────────────────────

(defmethod analyze-node* :texture-decl [node env]
  [node
   (env/add-texture env
                    (:name node)
                    (semantic/shader-texture-register node)
                    (semantic/shader-texture-dimension node)
                    (semantic/shader-texture-element-type node))])

(defmethod analyze-node* :sampler-decl [node env]
  [node
   (env/add-sampler env
                    (:name node)
                    (semantic/shader-sampler-register node)
                    (semantic/shader-sampler-compare? node))])

;; ── 采样调用 ────────────────────────────────

(defmethod analyze-node* :call [node env]
  (let [fn-name (:fn node)]

    (cond
      (lang/sample-fn? (:fn node))
      (let [args     (:args node)
            tex-name (var-name (first args))
            smp-name (var-name (second args))]
        [node
         (if (and tex-name smp-name)
           (env/add-sample-call env tex-name smp-name)
           env)])

      ;; 等价与 texelFetch
      (= (name fn-name) 'load)
      (let [tex-name (var-name (first (:args node)))]
        [node (env/add-sample-call env tex-name nil)])

      :else
      (ast/reduce-children node
                           (fn [child env] (analyze-node* child env))
                           env))))

;; ── 入口点 ──────────────────────────────────

(defmethod analyze-node* :entry-point [node env]
  (let [{:keys [name params body]} node
        stage           (semantic/shader-stage node)
        return-semantic (semantic/shader-semantic node)
        param-v         (mapv param->triple params)
        env'            (case stage
                          :compute
                          (throw
                            (ex-info "compute shader entry-point not yet supported"
                                     {:name name}))
                          (:vertex :fragment)
                          (env/add-entry env stage name
                                         (:return-type node)
                                         return-semantic
                                         param-v)
                          (throw
                            (ex-info "unsupported entry-point stage"
                                     {:stage stage :name name})))
        [_ env'']       (analyze-node* body env')]
    [node env'']))

;; ═══════════════════════════════════════════
;; 入口
;; ═══════════════════════════════════════════

(defn analyze
  "单遍遍历 shader AST 列表。返回 {:asts [node' ...] :env env'}。

   node' 通常等于原节点——本 pass 不修改 AST。
   env' 是累积后的环境，用 env/to-analysis 转成 SLAnalysis。"
  [asts init-env]
  (let [[roots final-env]
        (reduce
          (fn [[acc-asts acc-env] root]
            (let [[root' env'] (analyze-node* root acc-env)]
              [(conj acc-asts root') env']))
          [[] init-env]
          asts)]
    {:asts roots
     :env  final-env}))
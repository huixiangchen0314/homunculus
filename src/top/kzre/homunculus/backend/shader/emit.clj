(ns top.kzre.homunculus.backend.shader.emit
  "着色器通用代码生成器，基于 IR2 节点种类和 IShaderBackend 协议。"
  (:require [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.string :as str]
            [top.kzre.homunculus.core.types.model :as t]
            [top.kzre.homunculus.core.types.protocol :as tp]))

(defmulti emit
          (fn [node backend]
            (cond
              (satisfies? ir2p/INode node) (ir2p/kind node)
              (map? node)                   (:kind node)
              :else                         (throw (ex-info "Unsupported node type" {:node node})))))

(defmethod emit :default [node backend]
  (throw (ex-info (str "Unhandled node: " node) {:node node})))

(defn generate
  "生成完整着色器代码。基于类型检测区分资源与函数，委托给后端协议。
   entries 为入口描述列表，每个元素为 {:stage :vertex/:fragment, :fn-name \"...\"}。"
  [ir2-roots backend entries]
  (let [flat-roots    (mapcat (fn [r]
                                (if (= (ir2p/kind r) :block)
                                  (:exprs r)
                                  [r]))
                              ir2-roots)
        defines       (filter #(= (ir2p/kind %) :define) flat-roots)
        resource-type? (fn [ty]
                         (and ty (= (tp/type-kind ty) :con)
                              (contains? #{:texture2D :sampler :cbuffer} (:name ty))))
        resource-defs (filter #(resource-type? (get-in % [:val :attrs :type])) defines)
        ;; 全局常量：非资源、非 lambda（kind 不是 :lambda）、无 shader-stage
        global-defs   (filter #(and (= (ir2p/kind %) :define)
                                    (not (resource-type? (get-in % [:val :attrs :type])))
                                    (not (some? (some-> (ir2p/node-meta %) :shader-stage)))
                                    (let [val (:val %)]
                                      (not (and val (= (ir2p/kind val) :lambda)))))
                              defines)
        ;; 函数定义：val 为 lambda 且非资源（无论是否有 shader-stage）
        function-defs (remove #(or (resource-type? (get-in % [:val :attrs :type]))
                                   (let [val (:val %)]
                                     (not (and val (= (ir2p/kind val) :lambda)))))
                              defines)
        fn-def-map    (into {} (map (fn [d] [(name (:name d)) (emit d backend)]) function-defs))
        globals       (map #(emit % backend) resource-defs)
        global-decls  (for [d global-defs
                            :let [val (:val d)
                                  ir-type (get-in val [:attrs :type])
                                  init-expr (when (and val (not (= (ir2p/kind val) :lambda)))
                                              (emit val backend))]]
                        (sp/shader-global-decl backend (name (:name d)) ir-type init-expr))
        others        (remove #(= (ir2p/kind %) :define) flat-roots)
        entries'      (if (seq entries)
                        entries
                        (for [d function-defs
                              :let [stage (some-> (ir2p/node-meta d) :shader-stage)
                                    fn-name (:name d)]
                              :when stage]
                          {:stage stage :fn-name (name fn-name)}))
        entry-specs   (for [entry entries'
                            :let [stage   (:stage entry)
                                  fn-name (:fn-name entry)
                                  safe-fn (sp/shader-var-ref backend fn-name)
                                  existing-d (some #(when (= (name (:name %)) fn-name) %) function-defs)]]
                        ;; 如果手动提供了 input-params 或 output-params，直接使用，不再从函数提取
                        (if (or (:input-params entry) (:output-params entry))
                          {:stage         stage
                           :fn-name       safe-fn
                           :input-params  (or (:input-params entry) [])
                           :output-params (or (:output-params entry) [])}
                          ;; 否则按原有逻辑从函数定义生成
                          (if existing-d
                            (let [val          (:val existing-d)
                                  params       (:params val)
                                  raw-inputs   (if params
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
                                                       params)
                                                 [])
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
                              {:stage         stage
                               :fn-name       safe-fn
                               :input-params  input-params
                               :output-params (if output-param [output-param] [])})
                            ;; 无函数定义且无手动参数 → 临时函数
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
                              {:stage         stage
                               :fn-name       safe-fn
                               :input-params  []
                               :output-params []
                               :temp-fn       tmp-fn}))))
        final-fn-defs (concat (vals fn-def-map)
                              (keep :temp-fn entry-specs))
        final-specs   (mapv #(dissoc % :temp-fn) entry-specs)]
    (sp/shader-program backend final-fn-defs [] (concat globals global-decls) final-specs)))
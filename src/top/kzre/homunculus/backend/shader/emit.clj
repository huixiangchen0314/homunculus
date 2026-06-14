(ns top.kzre.homunculus.backend.shader.emit
  "着色器通用代码生成器，基于 IR2 节点种类和 IShaderBackend 协议。"
  (:require [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.string :as str]
            [top.kzre.homunculus.core.types.model :as t])
  (:import [top.kzre.homunculus.core.types.model TCon]
           [top.kzre.homunculus.core.ir2.model LambdaNode]))

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
        resource-type? (fn [vty]
                         (and vty (instance? TCon vty)
                              (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
        resource-defs (filter #(resource-type? (get-in % [:val :attrs :type])) defines)
        ;; 全局常量：非资源、非LambdaNode、无 shader-stage
        global-defs   (filter #(and (= (ir2p/kind %) :define)
                                    (not (resource-type? (get-in % [:val :attrs :type])))
                                    (not (some? (some-> (ir2p/node-meta %) :shader-stage)))
                                    (not (instance? LambdaNode (:val %))))
                              defines)
        ;; 函数定义：val 为 LambdaNode 且非资源（无论是否有 shader-stage）
        function-defs (remove #(or (resource-type? (get-in % [:val :attrs :type]))
                                   (not (instance? LambdaNode (:val %))))
                              defines)
        fn-def-map    (into {} (map (fn [d] [(name (:name d)) (emit d backend)]) function-defs))
        globals       (map #(emit % backend) resource-defs)
        global-decls  (for [d global-defs
                            :let [val (:val d)
                                  ir-type (get-in val [:attrs :type])
                                  init-expr (when (and val (not (instance? LambdaNode val)))
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
                                  safe-fn (sp/shader-var-ref backend fn-name)]]
                        (if-let [d (some #(when (= (name (:name %)) fn-name) %) function-defs)]
                          (let [val          (:val d)
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
                                                              nil)}))]
                            {:stage         stage
                             :fn-name       safe-fn
                             :input-params  input-params
                             :output-params (if output-param [output-param] [])})
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
                             :temp-fn       tmp-fn})))
        final-fn-defs (concat (vals fn-def-map)
                              (keep :temp-fn entry-specs))
        final-specs   (mapv #(dissoc % :temp-fn) entry-specs)]
    (sp/shader-program backend final-fn-defs [] (concat globals global-decls) final-specs)))
(ns top.kzre.homunculus.backend.shader.emit
  "着色器通用代码生成器，基于 IR2 节点种类和 IShaderBackend 协议。"
  (:require [top.kzre.homunculus.backend.shader.protocol :as sp]
            [top.kzre.homunculus.core.ir2.protocol :as ir2p]
            [clojure.string :as str]
            [top.kzre.homunculus.core.types.model :as t])
  (:import (top.kzre.homunculus.core.types.model TCon)))

(defmulti emit
          (fn [node backend]
            (cond
              (satisfies? ir2p/INode node) (ir2p/kind node)
              (map? node)                   (:kind node)
              :else                         (throw (ex-info "Unsupported node type" {:node node})))))

(defmethod emit :default [node backend]
  (throw (ex-info (str "Unhandled node: " node) {:node node})))

;; backend/shader/emit.clj 中的 generate 函数
;; emit.clj
(defn generate
  "生成完整着色器代码。基于类型检测区分资源与函数，委托给后端协议。
   entries 为入口描述列表，每个元素为 {:stage :vertex/:fragment, :fn-name \"...\"}。"
  [ir2-roots backend entries]
  (let [;; ═══ 1. 展平顶层 :block 节点 ═══
        flat-roots    (mapcat (fn [r]
                                (if (= (ir2p/kind r) :block)
                                  (:exprs r)
                                  [r]))
                              ir2-roots)
        ;; ═══ 2. 分类 ═══
        defines       (filter #(= (ir2p/kind %) :define) flat-roots)
        resource-type? (fn [vty]
                         (and vty (instance? TCon vty)
                              (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
        resource-defs (filter #(resource-type? (get-in % [:val :attrs :type])) defines)
        function-defs (remove #(resource-type? (get-in % [:val :attrs :type])) defines)
        ;; ═══ 3. 预先发射所有用户函数 ═══
        fn-def-map    (into {} (map (fn [d] [(name (:name d)) (emit d backend)]) function-defs))
        ;; 全局资源
        globals       (map #(emit % backend) resource-defs)
        ;; ═══ 4. 剩余非 define 节点（理论上现在为空） ═══
        others        (remove #(= (ir2p/kind %) :define) flat-roots)
        ;; ═══ 5. 确定入口列表 ═══
        entries'      (if (seq entries)
                        entries
                        (for [d function-defs
                              :let [stage (some-> (ir2p/node-meta d) :shader-stage)
                                    fn-name (:name d)]
                              :when stage]
                          {:stage stage :fn-name (name fn-name)}))
        ;; ═══ 6. 为每个入口构建 entry-spec ═══
        entry-specs   (for [entry entries'
                            :let [stage   (:stage entry)
                                  fn-name (:fn-name entry)
                                  safe-fn (sp/shader-var-ref backend fn-name)]]
                        (if-let [d (some #(when (= (name (:name %)) fn-name) %) function-defs)]
                          ;; 函数已定义 → 从参数提取 input/output 信息
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
                                ;; 片段着色器仅保留有语义的参数
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
                          ;; 函数未定义 → 临时函数（使用 others 作为函数体）
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
                                _ (println "WARNING: no def for" fn-name ", generating temp function")]
                            {:stage         stage
                             :fn-name       safe-fn
                             :input-params  []
                             :output-params []
                             ;; 返回临时函数定义字符串，同时将其加入最终函数列表
                             :temp-fn      (sp/shader-function-decl backend fn-name [] ret-type (or body-code ""))})))
        ;; ═══ 7. 收集最终函数定义 ═══
        final-fn-defs (concat (vals fn-def-map)
                              (keep :temp-fn entry-specs))
        final-specs   (mapv #(dissoc % :temp-fn) entry-specs)]
    (sp/shader-program backend final-fn-defs [] globals final-specs)))
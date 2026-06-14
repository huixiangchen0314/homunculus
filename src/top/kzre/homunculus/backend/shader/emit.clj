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
  (let [;; ═══════════════════════════════════════
        ;; 1. 展平顶层 :block 节点（处理 do 形式）
        flat-roots     (mapcat (fn [r]
                                 (if (= (ir2p/kind r) :block)
                                   (:exprs r)
                                   [r]))
                               ir2-roots)
        ;; ═══════════════════════════════════════
        defines        (filter #(= (ir2p/kind %) :define) flat-roots)
        resource-type? (fn [vty]
                         (and vty (instance? TCon vty)
                              (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
        resource-defs  (filter #(resource-type? (get-in % [:val :attrs :type])) defines)
        function-defs  (remove #(resource-type? (get-in % [:val :attrs :type])) defines)
        fn-def-map     (into {} (map (fn [d] [(name (:name d)) (emit d backend)]) function-defs))
        globals        (map #(emit % backend) resource-defs)
        others         (remove #(= (ir2p/kind %) :define) flat-roots)
        entries'       (if (seq entries)
                         entries
                         (for [d function-defs
                               :let [stage (some-> (ir2p/node-meta d) :shader-stage)
                                     fn-name (:name d)]
                               :when stage]
                           {:stage stage :fn-name (name fn-name)}))
        init-fn-defs   (vec (vals fn-def-map))
        [final-fn-defs entry-specs]
        (reduce
          (fn [[fdefs specs] entry]
            (let [stage   (:stage entry)
                  fn-name (:fn-name entry)
                  safe-fn (sp/shader-var-ref backend fn-name)]
              (if (contains? fn-def-map fn-name)
                ;; 函数已定义
                (let [d            (some #(when (= (name (:name %)) fn-name) %) function-defs)
                      val          (:val d)
                      params       (:params val)
                      input-params (if params
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
                      output-param (when val
                                     (when-let [ret-ty (get-in (:body val) [:attrs :type])]
                                       {:name     "pos"
                                        :type     (sp/shader-type backend ret-ty)
                                        :semantic (case stage
                                                    :vertex   "SV_POSITION"
                                                    :fragment "SV_TARGET"
                                                    nil)}))]
                  [fdefs
                   (conj specs {:stage         stage
                                :fn-name       safe-fn
                                :input-params  input-params
                                :output-params (if output-param [output-param] [])})])
                ;; 函数未定义，生成临时函数
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
                  [(conj fdefs tmp-fn)
                   (conj specs {:stage         stage
                                :fn-name       safe-fn
                                :input-params  []
                                :output-params []})]))))
          [init-fn-defs []]
          entries')]
    (sp/shader-program backend final-fn-defs [] globals entry-specs)))
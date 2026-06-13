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
  "生成完整着色器代码。基于类型检测区分资源与函数，委托给后端协议。"
  [ir2-roots backend entry-stage entry-fn-name]
  (let [defines        (filter #(= (ir2p/kind %) :define) ir2-roots)
        resource-type? (fn [vty]
                         (and vty (instance? TCon vty)
                              (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
        resource-defs  (filter #(resource-type? (get-in % [:val :attrs :type])) defines)
        function-defs  (remove #(resource-type? (get-in % [:val :attrs :type])) defines)
        fn-defs        (map #(emit % backend) function-defs)
        globals        (map #(emit % backend) resource-defs)
        others         (remove #(= (ir2p/kind %) :define) ir2-roots)
        ;; 从函数定义中提取入口参数信息（如果有）
        entry-spec     (when (seq function-defs)
                         (let [main-fn (first function-defs)
                               val     (:val main-fn)                  ; LambdaNode
                               params  (:params val)
                               input-params (mapv (fn [p]
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
                               output-param (when-let [ret-ty (get-in (:body val) [:attrs :type])]
                                              {:name     "pos" ; 默认输出名，后续可改进
                                               :type     (sp/shader-type backend ret-ty)
                                               :semantic (case entry-stage
                                                           :vertex   "SV_POSITION"
                                                           :fragment "SV_TARGET"
                                                           nil)})]
                           {:stage         entry-stage
                            :fn-name       (sp/shader-var-ref backend (:name main-fn))
                            :input-params  input-params
                            :output-params (if output-param [output-param] [])}))]
(if (seq function-defs)
  (sp/shader-program backend fn-defs [] globals (if entry-spec [entry-spec] []))
  ;; 无函数定义时，生成临时函数并包装
  (let [body-code (when (seq others)
                    (let [emitted   (map #(emit % backend) others)
                          last-idx  (dec (count emitted))
                          emitted'  (map-indexed (fn [i code]
                                                   (if (= i last-idx)
                                                     (sp/shader-return backend code)
                                                     code))
                                                 emitted)]
                      (sp/shader-block backend emitted')))
        ret-type  (case entry-stage :fragment "float4" "void")
        tmp-fn    (sp/shader-function-decl backend entry-fn-name [] ret-type (or body-code ""))
        tmp-spec  {:stage         entry-stage
                   :fn-name       (sp/shader-var-ref backend entry-fn-name)
                   :input-params  []
                   :output-params []}]
    (sp/shader-program backend [tmp-fn] [] globals [tmp-spec])))))
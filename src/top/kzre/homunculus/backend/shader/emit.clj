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
   entries 为入口描述列表，每个元素为 {:stage :vertex/:fragment, :fn-name \"...\"}。
   若 entries 为空，则从定义了 defshader 的函数中自动推导入口。"
  [ir2-roots backend entries]
  (let [defines        (filter #(= (ir2p/kind %) :define) ir2-roots)
        resource-type? (fn [vty]
                         (and vty (instance? TCon vty)
                              (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
        resource-defs  (filter #(resource-type? (get-in % [:val :attrs :type])) defines)
        function-defs  (remove #(resource-type? (get-in % [:val :attrs :type])) defines)
        fn-defs        (map #(emit % backend) function-defs)
        globals        (map #(emit % backend) resource-defs)
        others         (remove #(= (ir2p/kind %) :define) ir2-roots)
        ;; 如果未提供 entries，则从函数定义中查找带有 :shader-stage 元数据的函数
        entries'       (if (seq entries)
                         entries
                         (for [d function-defs
                               :let [stage (some-> (ir2p/node-meta d) :shader-stage)
                                     fn-name (:name d)]
                               :when stage]
                           {:stage stage :fn-name fn-name}))
        ;; 为每个入口构造 entry-spec，同时准备对应的函数定义字符串
        entry-specs+fn-defs
        (for [entry entries'
              :let [stage   (:stage entry)
                    fn-name (:fn-name entry)
                    ;; 在已定义的函数中查找匹配的函数
                    match-fn (some (fn [d] (when (= (name (:name d)) fn-name) d)) function-defs)
                    ;; 生成函数定义字符串：如果已定义则 emit，否则用 others 构建临时函数
                    fn-def-str (if match-fn
                                 (emit match-fn backend)
                                 ;; 没有定义，生成临时函数
                                 (let [body-code (when (seq others)
                                                   (let [emitted   (map #(emit % backend) others)
                                                         last-idx  (dec (count emitted))
                                                         emitted'  (map-indexed (fn [i code]
                                                                                  (if (= i last-idx)
                                                                                    (sp/shader-return backend code)
                                                                                    code))
                                                                                emitted)]
                                                     (sp/shader-block backend emitted')))
                                       ret-type  (case stage :fragment "float4" "void")]
                                   (sp/shader-function-decl backend fn-name [] ret-type (or body-code ""))))
                    ;; 提取参数信息（仅当 match-fn 存在时）
                    val          (when match-fn (:val match-fn))
                    params       (when val (:params val))
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
                                                  nil)}))]]
          {:entry-spec {:stage         stage
                        :fn-name       (sp/shader-var-ref backend fn-name)
                        :input-params  input-params
                        :output-params (if output-param [output-param] [])}
           :fn-def     fn-def-str})
        final-fn-defs (map :fn-def entry-specs+fn-defs)
        entry-specs   (map :entry-spec entry-specs+fn-defs)]
    (sp/shader-program backend final-fn-defs [] globals entry-specs)))
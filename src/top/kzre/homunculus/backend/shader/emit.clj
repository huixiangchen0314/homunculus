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
(defn generate
  "生成完整着色器代码。基于类型检测区分资源与函数，委托给后端协议。"
  [ir2-roots backend entry-stage entry-fn-name]
  (let [defines       (filter #(= (ir2p/kind %) :define) ir2-roots)
        ;; 提取资源检测谓词，避免重复
        resource-type? (fn [vty]
                         (and vty (instance? TCon vty)
                              (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
        resource-defs (filter #(resource-type? (get-in % [:val :attrs :type])) defines)
        function-defs (remove #(resource-type? (get-in % [:val :attrs :type])) defines)
        fn-defs       (map #(emit % backend) function-defs)
        globals       (map #(emit % backend) resource-defs)
        others        (remove #(= (ir2p/kind %) :define) ir2-roots)]
    (if (seq function-defs)
      (sp/shader-program backend fn-defs [] globals entry-stage entry-fn-name)
      ;; 只有表达式片段时：包装成临时函数
      (let [body-code (when (seq others)
                        (let [emitted (map #(emit % backend) others)
                              last-idx (dec (count emitted))
                              ;; 为最后一个简单表达式加 return
                              emitted' (map-indexed (fn [i code]
                                                      (if (= i last-idx)
                                                        (sp/shader-return backend code)
                                                        code))
                                                    emitted)]
                          (sp/shader-block backend emitted')))
            ;; 片段着色器固定返回 float4，其他 stage 暂用 void（可后续扩展）
            ret-type  (case entry-stage :fragment "float4" "void")
            tmp-fn    (sp/shader-function-decl backend entry-fn-name [] ret-type (or body-code ""))]
        (sp/shader-program backend [tmp-fn] [] globals entry-stage entry-fn-name)))))
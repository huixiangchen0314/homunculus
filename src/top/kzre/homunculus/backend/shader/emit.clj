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

(defn generate
  "生成完整着色器代码。基于类型检测区分资源与函数，委托给后端协议。"
  [ir2-roots backend entry-stage entry-fn-name]
  (let [defines       (filter #(= (ir2p/kind %) :define) ir2-roots)
        ;; 资源检测：看 val 的类型是否为资源类型
        resource-defs (filter #(let [v (:val %)
                                     vty (get-in v [:attrs :type])]
                                 (and vty (instance? TCon vty)
                                      (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
                              defines)
        function-defs (remove #(let [v (:val %)
                                     vty (get-in v [:attrs :type])]
                                 (and vty (instance? TCon vty)
                                      (contains? #{:texture2D :sampler :cbuffer} (:name vty))))
                              defines)
        fn-defs       (map #(emit % backend) function-defs)
        globals       (map #(emit % backend) resource-defs)
        others        (remove #(= (ir2p/kind %) :define) ir2-roots)]
    (if (seq function-defs)
      ;; 有明确函数定义：直接交给 shader-program
      (sp/shader-program backend fn-defs [] globals entry-stage entry-fn-name)
      ;; 只有表达式片段：包装成一个临时函数，再生成程序
      (let [body-code (when (seq others)
                        (let [exprs (map #(emit % backend) others)]
                          (if (= 1 (count exprs))
                            (sp/shader-return backend (first exprs))
                            (sp/shader-block backend exprs))))
            ;; 如果没有表达式，创建一个空函数体
            body-code' (or body-code "")
            tmp-fn     (sp/shader-function-decl backend entry-fn-name [] "void" body-code')]
        (sp/shader-program backend [tmp-fn] [] globals entry-stage entry-fn-name)))))